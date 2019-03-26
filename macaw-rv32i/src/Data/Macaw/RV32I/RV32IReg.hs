{-# LANGUAGE DataKinds #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE TypeOperators #-}

module Data.Macaw.RV32I.RV32IReg where

import Data.BitVector.Sized as BV
import Data.Macaw.CFG as MC
import Data.Macaw.Types ( BVType, HasRepr, TypeRepr(..), n32, typeRepr )
import Data.Parameterized
import Data.Parameterized.TH.GADT as TH

import Data.Macaw.RV32I.Arch

data RV32IReg tp where
  RV32I_GPR :: (w ~ MC.RegAddrWidth RV32IReg, 1 <= w) => BV.BitVector 5 -> RV32IReg (BVType w)
  RV32I_PC :: (w ~ MC.RegAddrWidth RV32IReg, 1 <= w) => RV32IReg (BVType w)

instance Show (RV32IReg tp) where
  show (RV32I_GPR bv) = "GPR[" ++ show bv ++ "]"
  show RV32I_PC = "<PC>"

instance ShowF RV32IReg where
  showF = show

$(return [])

instance TestEquality RV32IReg where
  testEquality = $(TH.structuralTypeEquality [t| RV32IReg |] [])

instance OrdF RV32IReg where
  compareF = $(TH.structuralTypeOrd [t| RV32IReg |] [])

instance HasRepr RV32IReg TypeRepr where
  typeRepr r =
    case r of
      RV32I_GPR {} -> BVTypeRepr n32
      RV32I_PC -> BVTypeRepr n32

type instance MC.ArchReg RV32I = RV32IReg
type instance MC.RegAddrWidth RV32IReg = 32

instance MC.RegisterInfo RV32IReg where
  archRegs = rv32iRegs
  sp_reg = RV32I_GPR 0x2
  ip_reg = RV32I_PC
  syscall_num_reg = error "MC.RegisterInfo RV32IReg syscall_num_reg undefined"
  syscallArgumentRegs = error "MC.RegisterInfo RG32IReg syscallArgumentsRegs undefined"

rv32iRegs :: forall w . (w ~ MC.RegAddrWidth RV32IReg, 1 <= w) => [Some RV32IReg]
rv32iRegs = [Some (RV32I_GPR n) | n <- [0..31] ] <>
            [Some RV32I_PC]
