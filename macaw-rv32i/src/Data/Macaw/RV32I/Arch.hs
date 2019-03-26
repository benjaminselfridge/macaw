{-# LANGUAGE DataKinds #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE KindSignatures #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE TypeOperators #-}

module Data.Macaw.RV32I.Arch where

import Data.Macaw.CFG as MC
import Data.Macaw.Types ( BVType, HasRepr, TypeRepr(..), n32, typeRepr, Type )
import Data.Parameterized

data RV32I

data RV32IPrimFn (f :: Type -> *) (tp :: Type) where
  QuotU :: (1 <= w) => NatRepr w
        -> f (BVType w)
        -> f (BVType w)
        -> RV32IPrimFn f (BVType w)
  QuotS :: (1 <= w) => NatRepr w
        -> f (BVType w)
        -> f (BVType w)
        -> RV32IPrimFn f (BVType w)
  RemU :: (1 <= w) => NatRepr w
       -> f (BVType w)
       -> f (BVType w)
       -> RV32IPrimFn f (BVType w)
  RemS :: (1 <= w) => NatRepr w
       -> f (BVType w)
       -> f (BVType w)
       -> RV32IPrimFn f (BVType w)

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
