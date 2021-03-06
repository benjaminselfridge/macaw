{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE GADTs #-}

module Data.Macaw.RISCV.Eval
  ( riscvInitialBlockRegs
  , riscvInitialAbsState
  , riscvCallParams
  ) where

import qualified Data.Macaw.AbsDomain.AbsState as MA
import qualified Data.Macaw.CFG as MC
import qualified Data.Macaw.Memory as MM
import qualified Data.Parameterized.Map as MapF
import qualified GRIFT.Types as G

import Control.Lens ((.~), (&))

import Data.Macaw.RISCV.Arch
import Data.Macaw.RISCV.RISCVReg

riscvInitialBlockRegs :: RISCV rv
                      => G.RVRepr rv
                      -> MC.ArchSegmentOff rv
                      -> MC.RegState (MC.ArchReg rv) (MC.Value rv ids)
riscvInitialBlockRegs rvRepr startIP = G.withRV rvRepr $
  MC.mkRegState MC.Initial &
  MC.curIP .~ MC.RelocatableValue (riscvAddrWidth rvRepr) (MM.segoffAddr startIP)

riscvInitialAbsState :: RISCV rv
                     => G.RVRepr rv
                     -> MM.Memory (MC.RegAddrWidth (MC.ArchReg rv))
                     -> MC.ArchSegmentOff rv
                     -> MA.AbsBlockState (MC.ArchReg rv)
riscvInitialAbsState rvRepr _mem startAddr = s0
  where
    initRegVals = MapF.fromList [ MapF.Pair ra MA.ReturnAddr ]
    s0 = G.withRV rvRepr $ MA.fnStartAbsBlockState startAddr initRegVals []

riscvCallParams :: MA.CallParams (RISCVReg rv)
riscvCallParams = MA.CallParams { MA.postCallStackDelta = 0
                                , MA.preserveReg = riscvPreserveReg
                                , MA.stackGrowsDown = True
                                }

riscvPreserveReg :: RISCVReg rv tp -> Bool
riscvPreserveReg (GPR rid)
  | rid == 2 = True
  | rid == 8 = True
  | rid == 9 = True
  | rid >= 18 && rid <= 27 = True
riscvPreserveReg (FPR rid)
  | rid == 8 = True
  | rid == 9 = True
  | rid >= 18 && rid <= 27 = True
riscvPreserveReg _ = False
