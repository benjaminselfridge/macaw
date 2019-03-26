{-# LANGUAGE DataKinds #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeOperators #-}

module Data.Macaw.RV32I.Translate
  ( initialBlockRegs
  , readInstruction
  , translateBlock
  ) where

import Control.Lens ((.~), (&))
import Control.Monad.Except
import Control.Monad.ST
import qualified Data.ByteString as BS
import Data.Foldable
import qualified Data.Macaw.AbsDomain.AbsState as MA
import Data.Macaw.CFG hiding ( BVValue )
import Data.Macaw.CFG.Block
import qualified Data.Macaw.Memory as MM
import Data.Parameterized
import Data.Parameterized.Nonce
import qualified Data.Sequence as Seq
import qualified GRIFT.Decode as G
import qualified GRIFT.InstructionSet as G
import qualified GRIFT.InstructionSet.Known as G
import qualified GRIFT.Semantics as G
import qualified GRIFT.Types as G

import Data.Macaw.RV32I.Arch
import Data.Macaw.RV32I.RV32IReg ()
import Data.Macaw.RV32I.Translate.Instruction

initialBlockRegs :: forall ids .
                    ArchSegmentOff RV32I
                 -> MA.AbsBlockState (ArchReg RV32I)
                 -> Either String (RegState (ArchReg RV32I) (Value RV32I ids))
initialBlockRegs blkAddr _abState = pure $ initRegState blkAddr

initRegState :: MM.MemSegmentOff (RegAddrWidth (ArchReg RV32I))
             -> RegState (ArchReg RV32I) (Value RV32I ids)
initRegState startPC =
  mkRegState Initial & curIP .~ RelocatableValue (addrWidthRepr startPC) (MM.segoffAddr startPC)

readInstruction :: MM.MemWidth w
                => MM.MemSegmentOff w
                -> Either (MM.MemoryError w) (Some (G.Instruction G.RV32I))
readInstruction addr = do
  contents <- MM.segoffContentsAfter addr
  case contents of
    [] -> throwError (MM.AccessViolation (MM.segoffAddr addr))
    MM.RelocationRegion r : _ -> throwError (MM.UnexpectedRelocation (MM.segoffAddr addr) r)
    MM.ByteRegion bs : _rest
      | BS.null bs -> throwError (MM.AccessViolation (MM.segoffAddr addr))
      | otherwise -> case G.bitVector <$> (BS.unpack $ BS.take 4 bs) of
          ([bv0, bv1, bv2, bv3] :: [G.BitVector 8]) -> do
            let bv = bv3 G.<:> bv2 G.<:> bv1 G.<:> bv0
            return $ G.decode (G.knownISet :: G.InstructionSet G.RV32I) bv
          _ -> throwError (MM.AccessViolation (MM.segoffAddr addr))
    MM.BSSRegion _ : _ -> throwError (MM.AccessViolation (MM.segoffAddr addr))

translateBlock  :: NonceGenerator (ST s) ids
                -> ArchSegmentOff RV32I
                -> RegState (ArchReg RV32I) (Value RV32I ids)
                -> Int
                -> ST s ([Block RV32I ids], Int, Maybe String)
translateBlock ng addr rs off = do
  (stmts, bytes, err, rs') <- translateBlock' ng addr rs off
  let block = Block undefined (toList stmts) (FetchAndExecute rs')
  return ([block], bytes, err)

translateBlock' :: NonceGenerator (ST s) ids
                -> ArchSegmentOff RV32I
                -> RegState (ArchReg RV32I) (Value RV32I ids)
                -> Int
                -> ST s ( Seq.Seq (Stmt RV32I ids)
                        , Int
                        , Maybe String
                        , RegState (ArchReg RV32I) (Value RV32I ids))
translateBlock' ng addr rs off = do
  case readInstruction addr of
    Left err -> return (Seq.empty, 0, Just (show err), rs)
    Right (Some inst@(G.Inst opcode _)) -> do
      transResult <- stmtsForInstruction ng inst rs
      case transResult of
        ITransError err _ -> return (Seq.empty, 0, Just (show err), rs)
        ITransSuccess stmtSeq (ITransState rs') -> do
          case isControlOpcode opcode of
            True -> return (stmtSeq, 4, Nothing, rs')
            False | Just nextAddr <- addr `MM.incSegmentOff` 4 -> do
              (rst, bytes, err, rs'') <- translateBlock' ng nextAddr rs' off
              return (stmtSeq <> rst, 4 + bytes, err, rs'')
            _ -> return (Seq.empty, 4, Just "couldn't increment PC", rs')

isControlOpcode :: G.Opcode G.RV32I fmt -> Bool
isControlOpcode G.Jalr = True
isControlOpcode G.Ecall = True
isControlOpcode G.Ebreak = True
isControlOpcode G.Beq = True
isControlOpcode G.Bne = True
isControlOpcode G.Blt = True
isControlOpcode G.Bge = True
isControlOpcode G.Bltu = True
isControlOpcode G.Bgeu = True
isControlOpcode G.Jal = True
isControlOpcode G.Illegal = True
isControlOpcode G.Mret = True
isControlOpcode _ = False
