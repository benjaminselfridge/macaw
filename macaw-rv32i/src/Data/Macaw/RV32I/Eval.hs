module Data.Macaw.RV32I.Eval
  ( mkInitialAbsState
  , absEvalArchFn
  , absEvalArchStmt
  , postCallAbsState
  ) where

import Control.Lens ((&), (.~))
import Data.Macaw.CFG
import qualified Data.Macaw.AbsDomain.AbsState as MA
import qualified Data.Macaw.Memory as MM

import Data.Macaw.RV32I.Arch
import Data.Macaw.RV32I.RV32IReg

-- | We use the x1/ra register as the return address. The calling convention
mkInitialAbsState :: MM.Memory (RegAddrWidth (ArchReg RV32I))
                  -> ArchSegmentOff RV32I
                  -> MA.AbsBlockState (ArchReg RV32I)
mkInitialAbsState _ startAddr =
  MA.top & MA.setAbsIP startAddr
         & MA.absRegState . boundValue (RV32I_GPR 1) .~ MA.ReturnAddr

absEvalArchFn :: MA.AbsProcessorState (ArchReg RV32I) ids
              -> ArchFn RV32I (Value RV32I ids) tp
              -> MA.AbsValue (RegAddrWidth (ArchReg RV32I)) tp
absEvalArchFn _ _f = MA.TopV

absEvalArchStmt :: MA.AbsProcessorState (ArchReg RV32I) ids
                -> ArchStmt RV32I (Value RV32I ids)
                -> MA.AbsProcessorState (ArchReg RV32I) ids
absEvalArchStmt aps _ = aps

postCallAbsState :: MA.AbsBlockState (ArchReg RV32I)
                 -> ArchSegmentOff RV32I
                 -> MA.AbsBlockState (ArchReg RV32I)
postCallAbsState = error "postCallAbsState"
