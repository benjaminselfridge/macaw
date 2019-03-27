module Data.Macaw.RV32I.Eval
  ( mkInitialAbsState
  , absEvalArchFn
  , absEvalArchStmt
  , postCallAbsState
  ) where

import Data.Macaw.CFG
import qualified Data.Macaw.AbsDomain.AbsState as MA
import qualified Data.Macaw.Memory as MM

import Data.Macaw.RV32I.Arch

mkInitialAbsState :: MM.Memory (RegAddrWidth (ArchReg RV32I))
                  -> ArchSegmentOff RV32I
                  -> MA.AbsBlockState (ArchReg RV32I)
mkInitialAbsState = undefined

absEvalArchFn :: MA.AbsProcessorState (ArchReg RV32I) ids
              -> ArchFn RV32I (Value RV32I ids) tp
              -> MA.AbsValue (RegAddrWidth (ArchReg RV32I)) tp
absEvalArchFn = undefined

absEvalArchStmt :: MA.AbsProcessorState (ArchReg RV32I) ids
                -> ArchStmt RV32I (Value RV32I ids)
                -> AbsProcessorState (ArchReg RV32I) ids
absEvalArchStmt = undefined

postCallAbsState :: AbsBlockState (ArchReg RV32I)
                 -> ArchSegmentOff RV32I
                 -> AbsBlockState (ArchReg RV32I)
postCallAbsState = undefined
