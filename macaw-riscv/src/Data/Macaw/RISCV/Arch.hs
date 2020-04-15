{-# LANGUAGE ConstraintKinds #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE TypeOperators #-}

module Data.Macaw.RISCV.Arch
  ( -- * RISC-V functions, statements, and terminators
    -- | There are none at this time.
    RISCVPrimFn
  , RISCVStmt
  , RISCVTermStmt
  , riscvPrimFnHasSideEffects
  , type RISCV
  ) where

import qualified Data.Kind as K
import qualified Data.Macaw.CFG as MC
import qualified Data.Macaw.Memory as MM
import qualified Data.Parameterized.TraversableF as F
import qualified Data.Parameterized.TraversableFC as FC
import qualified Data.Macaw.Types as MT
import qualified GRIFT.Types as G

-- | Macaw-specific constraints we need for the RISC-V configuration
-- type parameter.
type RISCV rv = ( MM.MemWidth (G.RVWidth rv)
                )

-- | RISC-V architecture-specific functions (none)
data RISCVPrimFn (rv :: G.RV) (expr :: MT.Type -> K.Type) (tp :: MT.Type)

instance FC.FoldableFC (RISCVPrimFn rv) where
  foldMapFC _ _ = undefined

instance MC.IsArchFn (RISCVPrimFn rv) where
  ppArchFn _ _ = undefined

type instance MC.ArchFn rv = RISCVPrimFn rv

-- | RISC-V architecture-specific statements (none)
data RISCVStmt (rv :: G.RV) (expr :: MT.Type -> K.Type)

instance F.FoldableF (RISCVStmt rv) where
  foldMapF _ _ = undefined

instance MC.IsArchStmt (RISCVStmt rv) where
  ppArchStmt _ _ = undefined

type instance MC.ArchStmt rv = RISCVStmt rv

-- | RISC-V block termination statements (none)
data RISCVTermStmt (rv :: G.RV) ids

instance MC.PrettyF (RISCVTermStmt rv) where
  prettyF = undefined

-- The IPAlignment instance will likely need to take computations like
-- this into account (for JAL):
--   x[rd] := pc + zext(step) pc := pc +
--   sext(imm20 << 0x1)
-- But for now, we leave it as trivial.
-- | This is an orphan instance because we are reusing the 'G.RV' type
-- from GRIFT.
instance MC.IPAlignment (rv :: G.RV) where
  fromIPAligned = Just
  toIPAligned = id

type instance MC.ArchTermStmt (rv :: G.RV) = RISCVTermStmt rv

type instance MC.ArchBlockPrecond (rv :: G.RV) = ()

riscvPrimFnHasSideEffects :: RISCVPrimFn rv f tp -> Bool
riscvPrimFnHasSideEffects _ = undefined
