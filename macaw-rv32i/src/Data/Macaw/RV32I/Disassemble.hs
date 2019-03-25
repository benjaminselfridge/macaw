{-# LANGUAGE DataKinds #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeOperators #-}

module Data.Macaw.RV32I.Disassemble
  ( initialBlockRegs
  ) where

import Control.Lens ((.~), (&), (^.))
import Control.Monad.Except
import Control.Monad.RWS
import Control.Monad.ST
-- import qualified Data.BitVector.Sized as BV
import qualified Data.Macaw.AbsDomain.AbsState as MA
import Data.Macaw.CFG hiding ( BVValue )
import qualified Data.Macaw.CFG as MC
import qualified Data.Macaw.Memory as MM
import qualified Data.Macaw.Types as MT
import Data.Parameterized
import Data.Parameterized.Nonce
import qualified Data.Parameterized.Map as MapF
import qualified Data.Sequence as Seq
import qualified GRIFT.Semantics as G
import qualified GRIFT.Types as G

import Data.Macaw.RV32I.Arch
import Data.Macaw.RV32I.RV32IReg

initialBlockRegs :: forall ids .
                    ArchSegmentOff RV32I
                 -> MA.AbsBlockState (ArchReg RV32I)
                 -> Either String (RegState (ArchReg RV32I) (Value RV32I ids))
initialBlockRegs blkAddr _abState = pure $ initRegState blkAddr

initRegState :: MM.MemSegmentOff (RegAddrWidth (ArchReg RV32I))
             -> RegState (ArchReg RV32I) (Value RV32I ids)
initRegState startPC =
  mkRegState Initial & curIP .~ RelocatableValue (addrWidthRepr startPC) (MM.segoffAddr startPC)

data ITransEnv fmt s ids = ITransEnv { iTransNonceGenerator :: NonceGenerator (ST s) ids
                                     , iTransOperandMap :: MapF.MapF (G.OperandID fmt) G.BitVector
                                     , iTransInstBytes :: Value RV32I ids (MT.BVType 32)
                                     , iTransInstWord :: Value RV32I ids (MT.BVType 32)
                                   }

data ITransState ids = ITransState { iTransStmts :: Seq.Seq (Stmt RV32I ids)
                                   , iTransRegState :: RegState (ArchReg RV32I) (Value RV32I ids)
                                   }

newtype BVValue ids w = BVValue { unBVValue :: Value RV32I ids (MT.BVType w) }

data ITransError fmt = OperandLookupError (Some (G.OperandID fmt))
                     | ZeroWidthBV

-- | Monad for translating instruction semantics into macaw statements.
newtype ITransM fmt s ids a = ITransM
  { unITransM :: ExceptT (ITransError fmt)
                 ( RWST
                   (ITransEnv fmt s ids)
                   (Seq.Seq (Stmt RV32I ids))
                   (ITransState ids)
                   (ST s) )
                 a
  }
  deriving ( Functor
           , Applicative
           , Monad
           , MonadReader (ITransEnv fmt s ids)
           , MonadState (ITransState ids)
           , MonadWriter (Seq.Seq (Stmt RV32I ids))
           , MonadError (ITransError fmt)
           )

liftST :: ST s a -> ITransM fmt s ids a
liftST sta = ITransM $ lift $ lift sta

---------------------
-- ITRANSM ACTIONS --
---------------------

getNonceGenerator :: ITransM fmt s ids (NonceGenerator (ST s) ids)
getNonceGenerator = iTransNonceGenerator <$> ask

getOperandMap :: ITransM fmt s ids (MapF.MapF (G.OperandID fmt) G.BitVector)
getOperandMap = iTransOperandMap <$> ask

getInstBytes :: ITransM fmt s ids (Value RV32I ids (MT.BVType 32))
getInstBytes = iTransInstBytes <$> ask

getInstWord :: ITransM fmt s ids (Value RV32I ids (MT.BVType 32))
getInstWord = iTransInstBytes <$> ask

getOperandValue :: G.OperandID fmt w
                -> ITransM fmt s ids (G.BitVector w)
getOperandValue oid = do
  operandMap <- getOperandMap
  case MapF.lookup oid operandMap of
    Nothing -> throwError $ OperandLookupError (Some oid)
    Just bv -> return bv

getRegState :: ITransM fmt s ids (RegState (ArchReg RV32I) (Value RV32I ids))
getRegState = iTransRegState <$> get

getRegValue :: ArchReg RV32I tp -> ITransM fmt s ids (Value RV32I ids tp)
getRegValue gpr = do
  regState <- getRegState
  return (regState ^. boundValue gpr)

addStmt :: Stmt RV32I ids -> ITransM fmt s ids ()
addStmt stmt = tell $ Seq.singleton stmt

assign :: AssignRhs RV32I (Value RV32I ids) (MT.BVType w)
       -> ITransM fmt s ids (Assignment RV32I ids (MT.BVType w))
assign rhs = do
  nonceGen <- getNonceGenerator
  nonce <- liftST $ freshNonce nonceGen
  let assignment = Assignment (AssignId nonce) rhs
  addStmt (AssignStmt assignment)
  return assignment

-----------------
-- TRANSLATION --
-----------------

withPosNat :: NatRepr w -> ((1 <= w) => a) -> ITransM fmt s ids a
withPosNat wRepr a = case isZeroOrGT1 wRepr of
  Left Refl -> throwError ZeroWidthBV
  Right LeqProof -> return a

withPosNatM :: NatRepr w -> ((1 <= w) => ITransM fmt s ids a) -> ITransM fmt s ids a
withPosNatM wRepr a = join (withPosNat wRepr a)

transInstExpr :: G.InstExpr fmt G.RV32I w
              -> ITransM fmt s ids (Value RV32I ids (MT.BVType w))
transInstExpr (G.InstLitBV (G.BitVector wRepr x)) = withPosNat wRepr $ MC.BVValue wRepr x
transInstExpr (G.OperandExpr wRepr oid) = withPosNatM wRepr $ do
  bv <- getOperandValue oid
  case bv of
    G.BitVector _ x -> return $ MC.BVValue wRepr x
transInstExpr (G.InstBytes _) = getInstBytes
transInstExpr (G.InstWord _) = getInstWord
transInstExpr (G.InstStateApp stateApp) = transStateApp stateApp

transStateApp :: G.StateApp (G.InstExpr fmt G.RV32I) G.RV32I w
              -> ITransM fmt s ids (Value RV32I ids (MT.BVType w))
transStateApp (G.LocApp locApp) = transLocApp locApp
transStateApp (G.AppExpr bvApp) = do
  assignment <- transBVApp bvApp
  return $ AssignedValue assignment

-- TODO: CSRs, privilege mode
transLocApp :: G.LocApp (G.InstExpr fmt G.RV32I) G.RV32I w
            -> ITransM fmt s ids (Value RV32I ids (MT.BVType w))
transLocApp (G.PCApp _) = getRegValue RV32I_PC
transLocApp (G.GPRApp _ (G.OperandExpr _ oid)) = do
  rid <- getOperandValue oid
  getRegValue (RV32I_GPR rid)
transLocApp (G.MemApp bytesRepr e) = withPosNatM bytesRepr $ do
  addr <- transInstExpr e
  memAssignment <- assign (ReadMem addr (BVMemRepr bytesRepr MM.LittleEndian))
  return (AssignedValue memAssignment)
transLocApp _ = undefined

transBVApp :: G.BVApp (G.InstExpr fmt G.RV32I) w
           -> ITransM fmt s ids (Assignment RV32I ids (MT.BVType w))
transBVApp bvApp = do
  bvApp <- traverseFC (return . BVValue <=< transInstExpr) bvApp
  macawApp <- bvToMacawApp bvApp
  assign (EvalApp macawApp)

bvToMacawApp :: forall fmt s ids w . G.BVApp (BVValue ids) w
             -> ITransM fmt s ids (App (Value RV32I ids) (MT.BVType w))
bvToMacawApp (G.AndApp wRepr e1 e2) = withPosNat wRepr $ BVAnd wRepr (unBVValue e1) (unBVValue e2)
bvToMacawApp (G.OrApp wRepr e1 e2) = withPosNat wRepr $ BVOr wRepr (unBVValue e1) (unBVValue e2)
bvToMacawApp (G.XorApp wRepr e1 e2) = withPosNat wRepr $ BVXor wRepr (unBVValue e1) (unBVValue e2)
bvToMacawApp (G.NotApp wRepr e) = withPosNat wRepr $ BVComplement wRepr (unBVValue e)
bvToMacawApp (G.SllApp wRepr e1 e2) = withPosNat wRepr $ BVShl wRepr (unBVValue e1) (unBVValue e2)
bvToMacawApp (G.SrlApp wRepr e1 e2) = withPosNat wRepr $ BVShr wRepr (unBVValue e1) (unBVValue e2)
bvToMacawApp (G.SraApp wRepr e1 e2) = withPosNat wRepr $ BVSar wRepr (unBVValue e1) (unBVValue e2)
bvToMacawApp (G.AddApp wRepr e1 e2) = withPosNat wRepr $ BVAdd wRepr (unBVValue e1) (unBVValue e2)
bvToMacawApp (G.SubApp wRepr e1 e2) = withPosNat wRepr $ BVSub wRepr (unBVValue e1) (unBVValue e2)
bvToMacawApp (G.MulApp wRepr e1 e2) = withPosNat wRepr $ BVMul wRepr (unBVValue e1) (unBVValue e2)
-- TODO: Handle these with architecture specific functions
  -- QuotUApp :: !(NatRepr w) -> !(expr w) -> !(expr w) -> BVApp expr w
  -- QuotSApp :: !(NatRepr w) -> !(expr w) -> !(expr w) -> BVApp expr w
  -- RemUApp  :: !(NatRepr w) -> !(expr w) -> !(expr w) -> BVApp expr w
  -- RemSApp  :: !(NatRepr w) -> !(expr w) -> !(expr w) -> BVApp expr w
  -- NegateApp :: !(NatRepr w) -> !(expr w) -> BVApp expr w
  -- AbsApp   :: !(NatRepr w) -> !(expr w) -> BVApp expr w
  -- SignumApp :: !(NatRepr w) -> !(expr w) -> BVApp expr w

  -- -- Comparisons
  -- EqApp  :: !(expr w) -> !(expr w) -> BVApp expr 1
  -- LtuApp :: !(expr w) -> !(expr w) -> BVApp expr 1
  -- LtsApp :: !(expr w) -> !(expr w) -> BVApp expr 1

  -- -- Width-changing
  -- ZExtApp    :: NatRepr w' -> !(expr w) -> BVApp expr w'
  -- SExtApp    :: NatRepr w' -> !(expr w) -> BVApp expr w'
  -- ExtractApp :: NatRepr w' -> Int -> !(expr w) -> BVApp expr w'
  -- ConcatApp  :: !(NatRepr (w+w')) -> !(expr w) -> !(expr w') -> BVApp expr (w+w')

  -- Other operations
  -- IteApp :: !(NatRepr w) -> !(expr 1) -> !(expr w) -> !(expr w) -> BVApp expr w
bvToMacawApp _ = undefined

-- transBVApp :: G.BVApp (BVValue ids) w
--            -> ITransM fmt s ids (Value RV32I ids (MT.BVType w))
-- transBVApp (G.LitBVApp (G.BitVector wRepr x)) = case isZeroOrGT1 wRepr of
--   Left Refl -> throwError ZeroWidthBV
--   Right LeqProof -> return $ MC.BVValue wRepr x
-- transBVApp
