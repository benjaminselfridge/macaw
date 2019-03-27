{-# LANGUAGE DataKinds #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeOperators #-}

module Data.Macaw.RV32I.Translate.Instruction
  ( stmtsForInstruction
  , ITransError(..)
  , ITransState(..)
  , ITransResult(..)
  ) where

import Control.Lens ((.~), (&), (^.))
import Control.Monad.Except
import Control.Monad.RWS
import Control.Monad.ST
import Data.Macaw.CFG hiding ( BVValue )
import qualified Data.Macaw.CFG as MC
import qualified Data.Macaw.Memory as MM
import qualified Data.Macaw.Types as MT
import Data.Parameterized
import qualified Data.Parameterized.List as PL
import Data.Parameterized.Nonce
import qualified GHC.TypeLits as T
import qualified Data.Sequence as Seq
import qualified GRIFT.Decode as G
import qualified GRIFT.InstructionSet as G
import qualified GRIFT.InstructionSet.Known as G
import qualified GRIFT.Semantics as G
import qualified GRIFT.Types as G

import Data.Macaw.RV32I.Arch
import Data.Macaw.RV32I.RV32IReg

data ITransResult fmt ids = ITransError (ITransError fmt) (ITransState ids)
                          | ITransSuccess (Seq.Seq (Stmt RV32I ids)) (ITransState ids)

-- | Translate an instruction into a list of macaw statements.
stmtsForInstruction :: NonceGenerator (ST s) ids
                    -> G.Instruction G.RV32I fmt
                    -> RegState (ArchReg RV32I) (Value RV32I ids)
                    -> ST s (ITransResult fmt ids)
stmtsForInstruction ng inst rs = do
  (e, st, stmts) <- runITransM (ITransEnv ng inst rs) (ITransState rs) transInstruction
  case e of
    Left err -> return $ ITransError err st
    Right () -> return $ ITransSuccess stmts st

-- | Environment for instruction translation.
data ITransEnv fmt s ids = ITransEnv { iTransNonceGenerator :: NonceGenerator (ST s) ids
                                       -- ^ A nonce generator for generating 'AssignId's.
                                     , iTransInstruction :: G.Instruction G.RV32I fmt
                                       -- ^ The instruction we are translating.
                                     , iTransInitialRegState :: RegState (ArchReg RV32I) (Value RV32I ids)
                                       -- ^ Initial state of the registers at the
                                       -- beginning of instruction execution. This is
                                       -- the only register state we should read from.
                                     }

-- | State for instruction translation.
data ITransState ids = ITransState { iTransCurrentRegState :: RegState (ArchReg RV32I) (Value RV32I ids)
                                     -- ^ Tracks the changing state of the registers
                                     -- as we execute the instruction. This should
                                     -- never be read from, so we might want to put
                                     -- it in the Writer monad at some point.
                                   }

newtype BVValue ids w = BVValue { unBVValue :: Value RV32I ids (MT.BVType w) }

data ITransError fmt = ZeroWidthBV
                     | UnsupportedLocError (Some (G.LocApp (G.InstExpr fmt G.RV32I) G.RV32I))
  deriving (Eq)

-- TODO: Fix this
instance Show (ITransError fmt) where
  show ZeroWidthBV = "zero-width bitvector"
  show (UnsupportedLocError _) = "unsupported location"

-- | Monad for translating the semantics of a single instruction in a sequence of
-- macaw statements.
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

-- | Execute an 'ITransM' action.
runITransM :: ITransEnv fmt s ids
           -> ITransState ids
           -> ITransM fmt s ids a
           -> ST s (Either (ITransError fmt) a, ITransState ids, Seq.Seq (Stmt RV32I ids))
runITransM env st action = runRWST (runExceptT (unITransM action)) env st

liftST :: ST s a -> ITransM fmt s ids a
liftST sta = ITransM $ lift $ lift sta

---------------------
-- ITRANSM ACTIONS --
---------------------

getNonceGenerator :: ITransM fmt s ids (NonceGenerator (ST s) ids)
getNonceGenerator = iTransNonceGenerator <$> ask

getInstruction ::ITransM fmt s ids (G.Instruction G.RV32I fmt)
getInstruction = iTransInstruction <$> ask

getInstWord :: ITransM fmt s ids (Value RV32I ids (MT.BVType 32))
getInstWord = do
  inst <- getInstruction
  let (G.BitVector _ x) = G.encode G.knownISet inst
  return $ MC.BVValue knownRepr x

getOperandValue :: G.OperandID fmt w -> ITransM fmt s ids (G.BitVector w)
getOperandValue oid = do
  G.Inst _ (G.Operands _ operandList) <- getInstruction
  return $ operandList PL.!! G.unOperandID oid

getInitialRegState :: ITransM fmt s ids (RegState (ArchReg RV32I) (Value RV32I ids))
getInitialRegState = iTransInitialRegState <$> ask

getInitialRegValue :: ArchReg RV32I tp -> ITransM fmt s ids (Value RV32I ids tp)
getInitialRegValue gpr = do
  regState <- getInitialRegState
  return (regState ^. boundValue gpr)

setCurPC :: Value RV32I ids (MT.BVType 32) -> ITransM fmt s ids ()
setCurPC pcVal = do
  currentRegState <- iTransCurrentRegState <$> get
  put $ ITransState (currentRegState & curIP .~ pcVal)

setCurGPR :: G.BitVector 5 -> Value RV32I ids (MT.BVType 32) -> ITransM fmt s ids ()
setCurGPR rid regVal = do
  currentRegState <- iTransCurrentRegState <$> get
  put $ ITransState (currentRegState & boundValue (RV32I_GPR rid) .~ regVal)

addStmt :: Stmt RV32I ids -> ITransM fmt s ids ()
addStmt stmt = tell $ Seq.singleton stmt

assign :: AssignRhs RV32I (Value RV32I ids) tp -> ITransM fmt s ids (Assignment RV32I ids tp)
assign rhs = do
  nonceGen <- getNonceGenerator
  nonce <- liftST $ freshNonce nonceGen
  let assignment = Assignment (AssignId nonce) rhs
  addStmt (AssignStmt assignment)
  return assignment

writeMem :: Value RV32I ids (MT.BVType 32)
         -> NatRepr bytes
         -> Value RV32I ids (MT.BVType (8 T.* bytes))
         -> ITransM fmt s ids ()
writeMem addr bytesRepr val = withPosNatM bytesRepr $
  addStmt $ WriteMem addr (BVMemRepr bytesRepr LittleEndian) val

-----------------
-- TRANSLATION --
-----------------

-- | Since grift/bv-sized allow zero-width bitvectors in their expression languages,
-- we use this function to throw an error if we ever encounter a zero-width bitvector
-- expression.
withPosNat :: NatRepr w -> ((1 <= w) => a) -> ITransM fmt s ids a
withPosNat wRepr a = case isZeroOrGT1 wRepr of
  Left Refl -> throwError ZeroWidthBV
  Right LeqProof -> return a

-- | Like 'withPosNat', but takes a monadic action rather than a pure value.
withPosNatM :: NatRepr w -> ((1 <= w) => ITransM fmt s ids a) -> ITransM fmt s ids a
withPosNatM wRepr a = join (withPosNat wRepr a)

-- | Every 'Value arch ids (BVType w)' has positive width, so we don't need a
-- 'NatRepr' to prove it to the compiler. This function case-splits on the two
-- constructors that produce a bitvector to eliminate that constraint.
withBVValuePosWidth :: ArchConstraints arch => Value arch ids (MT.BVType w) -> ((1 <= w) => a) -> a
withBVValuePosWidth (MC.BVValue _ _) a = a
withBVValuePosWidth (MC.RelocatableValue _ _) a = a
withBVValuePosWidth _ _ = error "withBVValuePosWidth"

-- | Translate the entire instruction. This should be called exactly once.
transInstruction :: ITransM fmt s ids ()
transInstruction = do
  G.Inst opcode _ <- getInstruction
  let sem = G.getInstSemantics $ G.semanticsFromOpcode G.knownISet opcode
      stmts = sem ^. G.semStmts
  mapM_ transStmt stmts

-- | Given a GRIFT 'G.Stmt', translate it to a macaw 'Stmt'.
transStmt :: G.Stmt (G.InstExpr fmt G.RV32I) G.RV32I -> ITransM fmt s ids ()
transStmt (G.AssignStmt (G.PCApp _) e) = transInstExpr e >>= setCurPC
transStmt (G.AssignStmt (G.GPRApp _ (G.OperandExpr _ oid)) e) = do
  rid <- getOperandValue oid
  v <- transInstExpr e
  setCurGPR rid v
transStmt (G.AssignStmt (G.MemApp bytesRepr addrE) e) = do
  addr <- transInstExpr addrE
  val <- transInstExpr e
  writeMem addr bytesRepr val
transStmt (G.AssignStmt locApp _) = throwError $ UnsupportedLocError (Some locApp)
transStmt _ = throwError $ undefined

-- | Given an 'G.InstExpr', translate it to a macaw 'Value' by traversing the
-- expression, building up a list of statements along the way.
transInstExpr :: G.InstExpr fmt G.RV32I w
              -> ITransM fmt s ids (Value RV32I ids (MT.BVType w))
transInstExpr (G.InstLitBV (G.BitVector wRepr x)) = withPosNat wRepr $ MC.BVValue wRepr x
transInstExpr (G.OperandExpr wRepr oid) = withPosNatM wRepr $ do
  bv <- getOperandValue oid
  case bv of
    G.BitVector _ x -> return $ MC.BVValue wRepr x
transInstExpr (G.InstBytes _) = return $ MC.BVValue knownRepr 4
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
transLocApp (G.PCApp _) = getInitialRegValue RV32I_PC
transLocApp (G.GPRApp _ (G.OperandExpr _ oid)) = do
  rid <- getOperandValue oid
  getInitialRegValue (RV32I_GPR rid)
transLocApp (G.MemApp bytesRepr e) = withPosNatM bytesRepr $ do
  addr <- transInstExpr e
  memAssignment <- assign (ReadMem addr (BVMemRepr bytesRepr MM.LittleEndian))
  return (AssignedValue memAssignment)
transLocApp locApp = throwError $ UnsupportedLocError (Some locApp)

transBVApp :: G.BVApp (G.InstExpr fmt G.RV32I) w
           -> ITransM fmt s ids (Assignment RV32I ids (MT.BVType w))
transBVApp bvApp = do
  bvApp' <- traverseFC (return . BVValue <=< transInstExpr) bvApp
  macawApp <- bvToMacawRhs bvApp'
  assign macawApp

bvToMacawRhs :: forall fmt s ids w . G.BVApp (BVValue ids) w
             -> ITransM fmt s ids (AssignRhs RV32I (Value RV32I ids) (MT.BVType w))
bvToMacawRhs (G.AndApp wRepr e1 e2) = withPosNat wRepr $ EvalApp $ BVAnd wRepr (unBVValue e1) (unBVValue e2)
bvToMacawRhs (G.OrApp wRepr e1 e2) = withPosNat wRepr $ EvalApp $ BVOr wRepr (unBVValue e1) (unBVValue e2)
bvToMacawRhs (G.XorApp wRepr e1 e2) = withPosNat wRepr $ EvalApp $ BVXor wRepr (unBVValue e1) (unBVValue e2)
bvToMacawRhs (G.NotApp wRepr e) = withPosNat wRepr $ EvalApp $ BVComplement wRepr (unBVValue e)
bvToMacawRhs (G.SllApp wRepr e1 e2) = withPosNat wRepr $ EvalApp $ BVShl wRepr (unBVValue e1) (unBVValue e2)
bvToMacawRhs (G.SrlApp wRepr e1 e2) = withPosNat wRepr $ EvalApp $ BVShr wRepr (unBVValue e1) (unBVValue e2)
bvToMacawRhs (G.SraApp wRepr e1 e2) = withPosNat wRepr $ EvalApp $ BVSar wRepr (unBVValue e1) (unBVValue e2)
bvToMacawRhs (G.AddApp wRepr e1 e2) = withPosNat wRepr $ EvalApp $ BVAdd wRepr (unBVValue e1) (unBVValue e2)
bvToMacawRhs (G.SubApp wRepr e1 e2) = withPosNat wRepr $ EvalApp $ BVSub wRepr (unBVValue e1) (unBVValue e2)
bvToMacawRhs (G.MulApp wRepr e1 e2) = withPosNat wRepr $ EvalApp $ BVMul wRepr (unBVValue e1) (unBVValue e2)
bvToMacawRhs (G.QuotUApp wRepr e1 e2) = withPosNat wRepr $ EvalArchFn (QuotU wRepr (unBVValue e1) (unBVValue e2)) (MT.BVTypeRepr wRepr)
bvToMacawRhs (G.QuotSApp wRepr e1 e2) = withPosNat wRepr $ EvalArchFn (QuotS wRepr (unBVValue e1) (unBVValue e2)) (MT.BVTypeRepr wRepr)
bvToMacawRhs (G.RemUApp wRepr e1 e2) = withPosNat wRepr $ EvalArchFn (RemU wRepr (unBVValue e1) (unBVValue e2)) (MT.BVTypeRepr wRepr)
bvToMacawRhs (G.RemSApp wRepr e1 e2) = withPosNat wRepr $ EvalArchFn (RemS wRepr (unBVValue e1) (unBVValue e2)) (MT.BVTypeRepr wRepr)
bvToMacawRhs (G.NegateApp wRepr e) = withPosNat wRepr $ EvalApp $ BVSub wRepr (MC.BVValue wRepr 0) (unBVValue e)
bvToMacawRhs (G.AbsApp wRepr e) = withPosNatM wRepr $ do
  t <- AssignedValue <$> (assign $ EvalApp $ BVSignedLe (MC.BVValue wRepr 0) (unBVValue e))
  neg <- AssignedValue <$> (assign $ EvalApp $ BVSub wRepr (MC.BVValue wRepr 0) (unBVValue e))
  return $ EvalApp $ Mux (MT.BVTypeRepr wRepr) t (unBVValue e) neg
bvToMacawRhs (G.SignumApp wRepr e) = withPosNatM wRepr $ do
  t <- AssignedValue <$> (assign $ EvalApp $ BVSignedLe (MC.BVValue wRepr 0) (unBVValue e))
  return $ EvalApp $ Mux (MT.BVTypeRepr wRepr) t (MC.BVValue wRepr 0) (MC.BVValue wRepr 1)
bvToMacawRhs (G.EqApp e1 e2) = do
  t <- AssignedValue <$> (assign $ EvalApp $ Eq (unBVValue e1) (unBVValue e2))
  return $ EvalApp $ Mux (MT.BVTypeRepr MT.n1) t (MC.BVValue MT.n1 1) (MC.BVValue MT.n1 0)
bvToMacawRhs (G.LtuApp e1 e2) = withBVValuePosWidth (unBVValue e1) $ do
  t <- AssignedValue <$> (assign $ EvalApp $ BVUnsignedLt (unBVValue e1) (unBVValue e2))
  return $ EvalApp $ Mux (MT.BVTypeRepr MT.n1) t (MC.BVValue MT.n1 1) (MC.BVValue MT.n1 0)
bvToMacawRhs (G.LtsApp e1 e2) = withBVValuePosWidth (unBVValue e1) $ do
  t <- AssignedValue <$> (assign $ EvalApp $ BVSignedLt (unBVValue e1) (unBVValue e2))
  return $ EvalApp $ Mux (MT.BVTypeRepr MT.n1) t (MC.BVValue MT.n1 1) (MC.BVValue MT.n1 0)
  -- ZExtApp    :: NatRepr w' -> !(expr w) -> BVApp expr w'
  -- SExtApp    :: NatRepr w' -> !(expr w) -> BVApp expr w'
  -- ExtractApp :: NatRepr w' -> Int -> !(expr w) -> BVApp expr w'
  -- ConcatApp  :: !(NatRepr (w+w')) -> !(expr w) -> !(expr w') -> BVApp expr (w+w')

  -- Other operations
  -- IteApp :: !(NatRepr w) -> !(expr 1) -> !(expr w) -> !(expr w) -> BVApp expr w
bvToMacawRhs _ = undefined
