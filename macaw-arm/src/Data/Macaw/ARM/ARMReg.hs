-- | Defines the register types and names/locations for ARM, along
-- with some helpers

{-# LANGUAGE DataKinds #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE StandaloneDeriving #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE UndecidableInstances #-}

module Data.Macaw.ARM.ARMReg
    ( ARMReg(..)
    , armRegToGPR
    , arm_LR
    -- , ArchWidth(..)
    , linuxSystemCallPreservedRegisters
    , locToRegTH
    )
    where

import qualified Data.Macaw.CFG as MC
import qualified Data.Macaw.Memory as MM
import           Data.Macaw.Types ( TypeRepr(..), HasRepr, BVType
                                  , typeRepr, n32 )
import           Data.Parameterized.Classes
import           Data.Parameterized.Some ( Some(..) )
import qualified Data.Parameterized.TH.GADT as TH
import qualified Data.Set as Set
import           Data.Word ( Word32 )
import qualified Dismantle.ARM.Operands as ARMOperands
import           GHC.TypeLits
import           Language.Haskell.TH
import           Language.Haskell.TH.Syntax ( lift )
import qualified SemMC.Architecture.AArch32 as ARM
import qualified SemMC.Architecture.ARM.Location as Loc
import qualified Text.PrettyPrint.HughesPJClass as PP

-- | Defines the Register set for the ARM processor.
data ARMReg tp where
    -- n.b. The Thumb (T32) register model is the same as the ARM
    -- (A32) model, so just use the latter to define registers.
    ARM_GP :: (w ~ MC.RegAddrWidth ARMReg, 1 <= w) => Word32 -> ARMReg (BVType w)
             -- GPR15 is normally aliased with the PC, but not always,
             -- so track it separately and use semantics definitions
             -- to manage the synchronization.

    -- | The Program Counter (often referred to by Macaw as the "IP"
    -- or Instruction Pointer, but ARM documentation refers to this as
    -- the PC).
    --
    -- Note that this is generally also GPR15, but there are
    -- oddities in the definition (see "Writing to the PC" in E.1.2.3,
    -- pg E1-2295, and particularly the note at the top of that page:
    -- "The A32 instruction set provides more general access to the
    -- PC, and many A32 instructions can use the PC as a
    -- general-purpose register.  However, ARM deprecates the use of
    -- PC for any purpose other than as the program counter."
    --
    -- Due to this ambiguity, the PC is currently modelled as separate
    -- and distinct from GPR15 in Macaw.  The SemMC semantics also
    -- make a distinction between GPR15 and PC and the semantics often
    -- simply update the latter (based on the ARM documentation),
    -- ignoring the former.
    ARM_PC :: (w ~ MC.RegAddrWidth ARMReg, 1 <= w) => ARMReg (BVType w)

    -- | The CPSR is the Current Processor Status Register, which is
    -- the current value of the Application Program Status Register
    -- (E1.2.4, page E1-2297).
    ARM_CPSR :: (w ~ MC.RegAddrWidth ARMReg, 1 <= w) => ARMReg (BVType w)

-- | GPR14 is the link register for ARM
arm_LR :: (w ~ MC.RegAddrWidth ARMReg, 1 <= w) => ARMReg (BVType w)
arm_LR = ARM_GP 14

armRegToGPR :: ARMReg tp -> Maybe ARMOperands.GPR
armRegToGPR (ARM_GP gp) = Just (ARMOperands.gpr gp)
armRegToGPR _ = Nothing

deriving instance Eq (ARMReg tp)
deriving instance Ord (ARMReg tp)

instance Show (ARMReg tp) where
    show r = case r of
               ARM_GP gpr_oper -> show $ PP.pPrint gpr_oper
                               --   case unGPRrnum of
                               -- 15 -> show rnum <> "=PC"
                               -- 14 -> show rnum <> "=LR"
                               -- 13 -> show rnum <> "=SP"
                               -- _ -> show rnum
               ARM_PC -> "<PC>"
               ARM_CPSR -> "<CPSR>"


instance ShowF ARMReg where
    showF = show

$(return [])  -- allow template haskell below to see definitions above

instance TestEquality ARMReg where
    testEquality = $(TH.structuralTypeEquality [t| ARMReg |] [])

instance OrdF ARMReg where
    compareF = $(TH.structuralTypeOrd [t| ARMReg |] [])

instance HasRepr ARMReg TypeRepr where
    typeRepr r =
        case r of
          ARM_GP {} -> BVTypeRepr n32
          ARM_PC -> BVTypeRepr n32
          ARM_CPSR -> BVTypeRepr n32


type instance MC.ArchReg ARM.AArch32 = ARMReg
type instance MC.RegAddrWidth ARMReg = 32


instance ( 1 <= MC.RegAddrWidth ARMReg
         , KnownNat (MC.RegAddrWidth ARMReg)
         , MM.MemWidth (MC.RegAddrWidth (MC.ArchReg ARM.AArch32))
         , MC.ArchReg ARM.AArch32 ~ ARMReg
         -- , ArchWidth arm
         ) =>
    MC.RegisterInfo ARMReg where
      archRegs = armRegs
      sp_reg = ARM_GP 13
      ip_reg = ARM_PC
      syscall_num_reg = error "MC.RegisterInfo ARMReg syscall_num_reg undefined"
      syscallArgumentRegs = error "MC.RegisterInfo ARMReg syscallArgumentsRegs undefined"

armRegs :: forall w. (w ~ MC.RegAddrWidth ARMReg, 1 <= w) => [Some ARMReg]
armRegs = [ Some (ARM_GP n) | n <- [0..ARM.numGPR-1] ] <>
          [ Some ARM_PC
          , Some ARM_CPSR
          ]


-- | The set of registers preserved across Linux system calls is defined by the ABI.
--
-- According to
-- https://stackoverflow.com/questions/12946958/system-call-in-arm,
-- R0-R6 are used to pass syscall arguments, r7 specifies the
-- syscall#, and r0 is the return code.
linuxSystemCallPreservedRegisters :: (w ~ MC.RegAddrWidth ARMReg, 1 <= w)
                                  => proxy arm
                                  -> Set.Set (Some ARMReg)
linuxSystemCallPreservedRegisters _ =
  Set.fromList [ Some (ARM_GP rnum) | rnum <- [8..ARM.numGPR-1] ]
  -- Currently, we are only considering the non-volatile GPRs.  There
  -- are also a set of non-volatile floating point registers.  I have
  -- to check on the vector registers.


-- | Translate a location from the semmc semantics into a location suitable for
-- use in macaw
locToRegTH :: (1 <= Loc.ArchRegWidth arm,
               MC.RegAddrWidth ARMReg ~ Loc.ArchRegWidth arm)
           => proxy arm
           -> Loc.Location arm ctp
           -> Q Exp
locToRegTH _  Loc.LocPC      = [| ARM_PC |]
locToRegTH _  (Loc.LocGPR g) = [| ARM_GP ($(lift g)) |]
locToRegTH _  (Loc.LocCPSR)  = [| ARM_CPSR |]
locToRegTH _  _              = [| error "locToRegTH undefined for unrecognized location" |]
