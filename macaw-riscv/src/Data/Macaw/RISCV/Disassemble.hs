{-# LANGUAGE DataKinds #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE TypeOperators #-}

module Data.Macaw.RISCV.Disassemble
  ( riscvDisassembleFn
  ) where

import qualified Control.Monad.Except as E
import qualified Control.Monad.RWS as RWS
import qualified Data.BitVector.Sized as BV
import qualified Data.ByteString as BS
import qualified Data.Foldable as F
import qualified Data.Macaw.CFG as MC
import qualified Data.Macaw.CFG.Block as MC
import qualified Data.Macaw.Memory as MM
import qualified Data.Macaw.Memory.Permissions as MMP
import qualified Data.Macaw.Types as MT
import qualified Data.Parameterized.List as L
import qualified Data.Parameterized.Map as MapF
import qualified Data.Sequence as Seq
import qualified GHC.TypeLits as T
import qualified GRIFT.InstructionSet as G
import qualified GRIFT.Decode as G
import qualified GRIFT.Semantics as G
import qualified GRIFT.Semantics.Expand as G
import qualified GRIFT.Types as G

import           Control.Lens (view, use, assign, makeLenses)
import           Control.Monad.ST (ST)
import           Control.Monad.Trans (lift)
import           Data.Parameterized.NatRepr
import           Data.Parameterized.Nonce ( NonceGenerator, freshNonce )
import           Data.Parameterized.Some (Some(..))
import           Data.Word (Word8)

import           Data.Macaw.RISCV.Arch
import           Data.Macaw.RISCV.RISCVReg

data RISCVMemoryError w = RISCVMemoryError !(MM.MemoryError w)

readBytesLE :: [Word8] -> Some BV.BitVector
readBytesLE [] = Some BV.bv0
readBytesLE (byte:rstBytes) =
  case readBytesLE rstBytes of
    Some rstBV ->
      let byteBV = BV.bitVector (fromIntegral byte) :: BV.BitVector 8
      in Some (rstBV BV.<:> byteBV)

bvWidth :: BV.BitVector w -> NatRepr w
bvWidth (BV.BitVector wRepr _) = wRepr

-- | Read a single RISC-V instruction.
readInstruction :: forall rv w . MM.MemWidth w
                => G.RVRepr rv
                -> G.InstructionSet rv
                -> MM.MemSegmentOff w
                -> Either (RISCVMemoryError w) (Some (G.Instruction rv), MM.MemWord w)
readInstruction rvRepr iset addr = do
  let seg = MM.segoffSegment addr
  case MM.segmentFlags seg `MMP.hasPerm` MMP.execute of
    False -> E.throwError (RISCVMemoryError (MM.PermissionsError (MM.segoffAddr addr)))
    True -> do
      contents <- liftMemError $ MM.segoffContentsAfter addr
      case contents of
        [] -> E.throwError (RISCVMemoryError (MM.AccessViolation (MM.segoffAddr addr)))
        MM.RelocationRegion r : _ ->
          E.throwError (RISCVMemoryError (MM.UnexpectedRelocation (MM.segoffAddr addr) r))
        MM.BSSRegion {} : _ ->
          E.throwError (RISCVMemoryError (MM.UnexpectedBSS (MM.segoffAddr addr)))
        MM.ByteRegion bs : _rest
          | BS.null bs -> E.throwError (RISCVMemoryError (MM.AccessViolation (MM.segoffAddr addr)))
          | otherwise -> do
              case rvRepr of
                -- If the C extension is present, first attempt to
                -- decode 2 bytes. If that fails, decode 4 bytes.
                G.RVRepr _ (G.ExtensionsRepr _ _ _ _ G.CYesRepr) -> do
                  cinstBV <- case readBytesLE (BS.unpack (BS.take 2 bs)) of
                    Some cinstBV | NatCaseEQ <- testNatCases (bvWidth cinstBV) (knownNat @16) -> return cinstBV
                    _ -> E.throwError (RISCVMemoryError (MM.AccessViolation (MM.segoffAddr addr)))
                  case G.decodeC rvRepr cinstBV of
                    Just sinst -> return (sinst, 2)
                    Nothing -> do
                      instBV <- case readBytesLE (BS.unpack (BS.take 4 bs)) of
                        Some instBV | NatCaseEQ <- testNatCases (bvWidth instBV) (knownNat @32) -> return instBV
                        _ -> E.throwError (RISCVMemoryError (MM.AccessViolation (MM.segoffAddr addr)))
                      return (G.decode iset instBV, 4)
                _ -> do
                  instBV <- case readBytesLE (BS.unpack (BS.take 4 bs)) of
                    Some instBV | NatCaseEQ <- testNatCases (bvWidth instBV) (knownNat @32) -> return instBV
                    _ -> E.throwError (RISCVMemoryError (MM.AccessViolation (MM.segoffAddr addr)))
                  return (G.decode iset instBV, 4)

liftMemError :: Either (MM.MemoryError w) a -> Either (RISCVMemoryError w) a
liftMemError e =
  case e of
    Left err -> Left (RISCVMemoryError err)
    Right a -> Right a

------------

data DisInstEnv s ids rv fmt = DisInstEnv { _disInst :: G.Instruction rv fmt
                                          , _disInstBytes :: Integer
                                          , _disInstWord :: Integer
                                          , _disNonceGen :: NonceGenerator (ST s) ids
                                          }
makeLenses ''DisInstEnv

data DisInstState (rv :: G.RV) ids = DisInstState { _disRegState :: MC.RegState (MC.ArchReg rv) (MC.Value rv ids)
                                                  }
makeLenses ''DisInstState

data DisInstError rv fmt = NonConstantGPR (G.InstExpr fmt rv 5)
                         | NonConstantFPR (G.InstExpr fmt rv 5)
                         | ZeroWidthExpr (G.InstExpr fmt rv 0)
                         | forall w w' . WidthNotLTExpr (G.InstExpr fmt rv w) (NatRepr w')

-- | Monad for disassembling a single instruction.
newtype DisInstM s ids rv fmt a = DisInstM
  { unDisInstM :: E.ExceptT (DisInstError rv fmt) (RWS.RWST (DisInstEnv s ids rv fmt) (Seq.Seq (MC.Stmt rv ids)) (DisInstState rv ids) (ST s)) a }
  deriving ( Functor
           , Applicative
           , Monad
           , RWS.MonadReader (DisInstEnv s ids rv fmt)
           , RWS.MonadWriter (Seq.Seq (MC.Stmt rv ids))
           , RWS.MonadState (DisInstState rv ids)
           , E.MonadError (DisInstError rv fmt)
           )

widthPos :: G.InstExpr fmt rv w -> (1 <= w => DisInstM s ids rv fmt a) -> DisInstM s ids rv fmt a
widthPos e a = case isZeroOrGT1 (G.exprWidth e) of
  Left Refl -> E.throwError (ZeroWidthExpr e)
  Right LeqProof -> a

widthLt :: G.InstExpr fmt rv w -> NatRepr w' -> ((w + 1) <= w' => DisInstM s ids rv fmt a) -> DisInstM s ids rv fmt a
widthLt e w a = case testNatCases (G.exprWidth e) w of
  NatCaseLT LeqProof -> a
  _ -> E.throwError (WidthNotLTExpr e w)

liftST :: ST s a -> DisInstM s ids rv fmt a
liftST = DisInstM . lift . lift

addAssignment :: MC.AssignRhs rv (MC.Value rv ids) tp
              -> DisInstM s ids rv fmt (MC.Value rv ids tp)
addAssignment rhs = do
  ng <- view disNonceGen
  id <- liftST $ freshNonce ng
  let asgn = MC.Assignment (MC.AssignId id) rhs
  addStmt (MC.AssignStmt asgn)
  return (MC.AssignedValue asgn)

evalApp :: MC.App (MC.Value rv ids) tp
        -> DisInstM s ids rv fmt (MC.Value rv ids tp)
evalApp = addAssignment . MC.EvalApp

readMem :: MC.Value rv ids (MT.BVType (MC.ArchAddrWidth rv))
        -> MC.MemRepr tp
        -> DisInstM s ids rv fmt (MC.Value rv ids tp)
readMem addr bytes = addAssignment $ MC.ReadMem addr bytes

getReg :: MC.ArchReg rv tp -> DisInstM s ids rv fmt (MC.Value rv ids tp)
getReg r = use (disRegState . MC.boundValue r)

setReg :: MC.ArchReg rv tp -> MC.Value rv ids tp -> DisInstM s ids rv fmt ()
setReg r v = assign (disRegState . MC.boundValue r) v

addStmt :: MC.Stmt rv ids -> DisInstM s ids rv fmt ()
addStmt stmt = RWS.tell (Seq.singleton stmt)

writeMem :: MC.ArchAddrValue rv ids
         -> MC.MemRepr tp
         -> MC.Value rv ids tp
         -> DisInstM s ids rv fmt ()
writeMem addr bytes val = addStmt (MC.WriteMem addr bytes val)

disLocApp :: (1 <= w, RISCV rv)
          => G.LocApp (G.InstExpr fmt rv) rv w
          -> DisInstM s ids rv fmt (MC.Value rv ids (MT.BVType w))
disLocApp locApp = case locApp of
  G.PCApp _w -> getReg PC
  G.GPRApp _w ridExpr -> do
    rid <- disInstExpr ridExpr
    case rid of
      MC.BVValue _w ridVal -> getReg (GPR (BV.bitVector ridVal))
      _ -> E.throwError (NonConstantGPR ridExpr)
  G.FPRApp _w ridExpr -> do
    rid <- disInstExpr ridExpr
    case rid of
      MC.BVValue _w ridVal -> getReg (FPR (BV.bitVector ridVal))
      _ -> E.throwError (NonConstantFPR ridExpr)
  G.MemApp bytes addrExpr -> do
    addr <- disInstExpr addrExpr
    readMem addr (MC.BVMemRepr bytes MC.LittleEndian)
  G.ResApp _addr -> error "TODO: disassemble ResApp"
  G.CSRApp _w _rid -> error "TODO: disassemble CSRApp"
  G.PrivApp -> getReg PrivLevel

disBVApp :: (1 <= w, RISCV rv)
         => G.BVApp (G.InstExpr fmt rv) w
         -> DisInstM s ids rv fmt (MC.Value rv ids (MT.BVType w))
disBVApp bvApp = case bvApp of
  G.AndApp w e1 e2 -> binaryOp (MC.BVAnd w) e1 e2
  G.OrApp w e1 e2 -> binaryOp (MC.BVOr w) e1 e2
  G.XorApp w e1 e2 -> binaryOp (MC.BVXor w) e1 e2
  G.NotApp w e -> unaryOp (MC.BVComplement w) e
  G.SllApp w e1 e2 -> binaryOp (MC.BVShl w) e1 e2
  G.SrlApp w e1 e2 -> binaryOp (MC.BVShr w) e1 e2
  G.SraApp w e1 e2 -> binaryOp (MC.BVSar w) e1 e2
  G.AddApp w e1 e2 -> binaryOp (MC.BVAdd w) e1 e2
  G.SubApp w e1 e2 -> binaryOp (MC.BVSub w) e1 e2
  G.MulApp w e1 e2 -> binaryOp (MC.BVMul w) e1 e2
  G.QuotUApp _w _e1 _e2 -> error "TODO: Disassemble QuotUApp"
  G.QuotSApp _w _e1 _e2 -> error "TODO: Disassemble QuotSApp"
  G.RemUApp _w _e1 _e2 -> error "TODO: Disassemble RemUApp"
  G.RemSApp _w _e1 _e2 -> error "TODO: Disassemble RemSApp"
  G.NegateApp _w _e -> error "TODO: Disassemble NegateApp"
  G.AbsApp _w _e -> error "TODO: Disassemble AbsApp"
  G.SignumApp _w _e -> error "TODO: Disassemble SignumApp"
  G.EqApp e1 e2 -> widthPos e1 $ binaryOpBool MC.Eq e1 e2
  G.LtuApp e1 e2 -> widthPos e1 $ binaryOpBool MC.BVSignedLt e1 e2
  G.LtsApp e1 e2 -> widthPos e1 $ binaryOpBool MC.BVUnsignedLt e1 e2
  -- TODO: The following two cases should use either extension or
  -- truncation depending on what the widths of the vectors are. They
  -- should never throw an error.
  G.ZExtApp _w _e -> error "TODO: Disassemble ZExtApp"
  G.SExtApp _w _e -> error "TODO: Disassemble SExtApp"
  G.ConcatApp _w _e1 _e2 -> error "TODO: Disassemble ConcatApp"
  G.IteApp w test e1 e2 -> do
    testVal <- disInstExpr test
    testValBool <- bvToBool testVal
    e1Val <- disInstExpr e1
    e2Val <- disInstExpr e2
    addAssignment (MC.EvalApp (MC.Mux (MT.BVTypeRepr w) testValBool e1Val e2Val))
  where unaryOp op e = do
          eVal <- disInstExpr e
          addAssignment (MC.EvalApp (op eVal))
        binaryOp op e1 e2 = do
          e1Val <- disInstExpr e1
          e2Val <- disInstExpr e2
          addAssignment (MC.EvalApp (op e1Val e2Val))
        binaryOpBool op e1 e2 = do
          eq <- binaryOp op e1 e2
          boolToBV eq
        boolToBV bool = addAssignment (MC.EvalApp (MC.Mux (MT.BVTypeRepr (knownNat @1)) bool
                                                   (MC.BVValue knownNat 1)
                                                   (MC.BVValue knownNat 0)))
        bvToBool bv = addAssignment (MC.EvalApp (MC.Eq bv (MC.BVValue (knownNat @1) 1)))

disStateApp :: (1 <= w, RISCV rv)
            => G.StateApp (G.InstExpr fmt rv) rv w
            -> DisInstM s ids rv fmt (MC.Value rv ids (MT.BVType w))
disStateApp stateApp = case stateApp of
  G.LocApp locApp -> disLocApp locApp
  G.AppExpr bvApp -> disBVApp bvApp
  G.FloatAppExpr _flApp -> error "TODO: Disassemble FloatAppExpr"

instOperand :: G.OperandID fmt w -> G.Instruction rv fmt -> BV.BitVector w
instOperand (G.OperandID oix) (G.Inst _ (G.Operands _ operands)) = operands L.!! oix

disInstExpr :: (1 <= w, RISCV rv)
            => G.InstExpr fmt rv w
            -> DisInstM s ids rv fmt (MC.Value rv ids (MT.BVType w))
disInstExpr instExpr = case instExpr of
  G.InstLitBV (BV.BitVector w val) -> return (MC.BVValue w val)
  G.InstAbbrevApp abbrevApp -> disInstExpr (G.expandAbbrevApp abbrevApp)
  G.OperandExpr w oid -> do
    inst <- view disInst
    let BV.BitVector _ val = instOperand oid inst
    return (MC.BVValue w val)
  G.InstBytes w -> do
    instBytes <- view disInstBytes
    return (MC.BVValue w instBytes)
  G.InstWord w -> do
    instWord <- view disInstWord
    return (MC.BVValue w instWord)
  G.InstStateApp stateApp -> disStateApp stateApp

data AssignStmt expr rv = forall w . AssignStmt !(G.LocApp (expr rv) rv w) !(expr rv w)

-- | Convert a 'G.Stmt' into a list of assignment statements. It isn't
-- clear what happens if the assignments overlap, so we make the
-- assumption that they don't for now (which should be mostly the
-- case, as branch statements are mostly used for exception handling).
collapseStmt :: RISCV rv => G.Stmt (G.InstExpr fmt) rv -> [AssignStmt (G.InstExpr fmt) rv]
collapseStmt stmt = case stmt of
  G.AssignStmt loc e -> [AssignStmt loc e]
  G.AbbrevStmt abbrevStmt -> mconcat (F.toList (collapseStmt <$> (G.expandAbbrevStmt abbrevStmt)))
  G.BranchStmt testExpr lStmts rStmts ->
    let lAssignStmts = mconcat (F.toList (collapseStmt <$> F.toList lStmts))
        rAssignStmts = mconcat (F.toList (collapseStmt <$> F.toList rStmts))
    in collectBranch testExpr lAssignStmts rAssignStmts
  where collectBranch testExpr lAssignStmts rAssignStmts =
          [ AssignStmt loc (G.iteE testExpr e (G.stateExpr (G.LocApp loc))) | AssignStmt loc e <- lAssignStmts ] ++
          [ AssignStmt loc (G.iteE testExpr (G.stateExpr (G.LocApp loc)) e) | AssignStmt loc e <- lAssignStmts ]

disAssignStmt :: RISCV rv => AssignStmt (G.InstExpr fmt) rv -> DisInstM s ids rv fmt ()
disAssignStmt stmt = case stmt of
  AssignStmt (G.PCApp _) valExpr -> do
    val <- disInstExpr valExpr
    setReg PC val
  AssignStmt (G.MemApp bytes addrExpr) valExpr -> withLeqProof (leqMulPos (knownNat @8) bytes) $ do
    addr <- disInstExpr addrExpr
    val <- disInstExpr valExpr
    writeMem addr (MC.BVMemRepr bytes MC.LittleEndian) val
  AssignStmt _ _ -> undefined

-- | Translate a GRIFT assignment statement into Macaw statement(s).
disStmt :: RISCV rv => G.Stmt (G.InstExpr fmt) rv -> DisInstM s ids rv fmt ()
disStmt stmt = F.traverse_ disAssignStmt (collapseStmt stmt)

riscvDisassembleFn :: G.RVRepr rv
                   -> NonceGenerator (ST s) ids
                   -> MC.ArchSegmentOff rv
                   -> MC.RegState (MC.ArchReg rv) (MC.Value rv ids)
                   -> Int
                   -> ST s (MC.Block rv ids, Int)
riscvDisassembleFn _rvRepr _nonceGen _startAddr _regState _maxSize = do
  undefined
