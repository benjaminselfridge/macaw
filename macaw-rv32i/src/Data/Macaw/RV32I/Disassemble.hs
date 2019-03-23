{-# LANGUAGE RankNTypes #-}

module Data.Macaw.RV32I.Disassemble
  ( initialBlockRegs
  ) where

import Control.Lens ((.~), (&))
import Data.Macaw.AbsDomain.AbsState as MA
import Data.Macaw.CFG
import Data.Macaw.Memory as MM

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
