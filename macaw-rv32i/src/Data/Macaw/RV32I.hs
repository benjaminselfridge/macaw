{-# LANGUAGE DataKinds #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE TypeOperators #-}

module Data.Macaw.RV32I
  ( -- * Macaw configurations
    rv32i_linux_info
    -- * Type-level tags
  , RV32I
  ) where

import Data.BitVector.Sized as BV
import Data.Macaw.Architecture.Info as MI
import Data.Macaw.CFG as MC
import Data.Macaw.Memory as MM
import Data.Macaw.Types ( BVType, HasRepr, TypeRepr(..), n32, typeRepr, Type )
import Data.Parameterized
import qualified Data.Parameterized.TH.GADT as TH
import GHC.TypeLits
import qualified GRIFT.Types as G

data RV32I

rv32i_linux_info :: MI.ArchitectureInfo RV32I
rv32i_linux_info =
  MI.ArchitectureInfo { MI.withArchConstraints = \x -> x
                      , MI.archAddrWidth = MM.Addr32
                      , MI.archEndianness = MM.LittleEndian
                      , MI.mkInitialRegsForBlock = undefined
                      , MI.disassembleFn = undefined
                      , MI.mkInitialAbsState = undefined
                      , MI.absEvalArchFn = undefined
                      , MI.absEvalArchStmt = undefined
                      , MI.postCallAbsState = undefined
                      , MI.identifyCall = undefined
                      , MI.checkForReturnAddr = undefined
                      , MI.identifyReturn = undefined
                      , MI.rewriteArchFn = undefined
                      , MI.rewriteArchStmt = undefined
                      , MI.rewriteArchTermStmt = undefined
                      , MI.archDemandContext = undefined
                      , MI.postArchTermStmtAbsState = undefined
                      }

data RV32IReg tp where
  RV32I_GPR :: (w ~ MC.RegAddrWidth RV32IReg, 1 <= w) => BV.BitVector 5 -> RV32IReg (BVType w)
  RV32I_PC :: (w ~ MC.RegAddrWidth RV32IReg, 1 <= w) => RV32IReg (BVType w)

instance Show (RV32IReg tp) where
  show = undefined

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
  archRegs = undefined
  sp_reg = undefined
  ip_reg = undefined
  syscall_num_reg = undefined
  syscallArgumentRegs = undefined

data RV32IPrimFn (f :: Type -> *) (tp :: Type)

instance MC.IsArchFn RV32IPrimFn where
  ppArchFn _ = undefined

instance FunctorFC RV32IPrimFn where
  fmapFC = fmapFCDefault

instance FoldableFC RV32IPrimFn where
  foldMapFC = foldMapFCDefault

instance TraversableFC RV32IPrimFn where
  traverseFC _ _ = undefined

type instance MC.ArchFn RV32I = RV32IPrimFn

data RV32IStmt (v :: Type -> *)

instance FunctorF RV32IStmt where
  fmapF = fmapFDefault


instance FoldableF RV32IStmt where
  foldMapF = foldMapFDefault

instance TraversableF RV32IStmt where
  traverseF _ _ = undefined

type instance MC.ArchStmt RV32I = RV32IStmt

instance MC.IsArchStmt RV32IStmt where
  ppArchStmt _ = undefined

data RV32ITermStmt ids

type instance MC.ArchTermStmt RV32I = RV32ITermStmt

instance MC.PrettyF RV32ITermStmt where
  prettyF _ = undefined

instance MC.IPAlignment RV32I where
  fromIPAligned _ = Nothing
  toIPAligned = undefined
