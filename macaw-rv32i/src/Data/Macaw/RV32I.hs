{-# LANGUAGE DataKinds #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE TypeOperators #-}

module Data.Macaw.RV32I
  ( -- * Macaw configurations
    rv32i_linux_info
    -- * Type-level tags
  , RV32I
  ) where

import Data.Macaw.Architecture.Info as MI
import Data.Macaw.Memory as MM

import Data.Macaw.RV32I.Arch
import Data.Macaw.RV32I.Translate

rv32i_linux_info :: MI.ArchitectureInfo RV32I
rv32i_linux_info =
  MI.ArchitectureInfo { MI.withArchConstraints = \x -> x
                      , MI.archAddrWidth = MM.Addr32
                      , MI.archEndianness = MM.LittleEndian
                      , MI.mkInitialRegsForBlock = initialBlockRegs
                      , MI.disassembleFn = translateBlock
                      , MI.mkInitialAbsState = undefined
                      , MI.absEvalArchFn = undefined
                      , MI.absEvalArchStmt = undefined
                      , MI.postCallAbsState = undefined
                      , MI.identifyCall = undefined
                      , MI.checkForReturnAddr = undefined
                      , MI.identifyReturn = undefined
                      , MI.rewriteArchFn = undefined
                      , MI.rewriteArchStmt = undefined
                      , MI.rewriteArchTermStmt = undefined
                      , MI.archDemandContext = undefined
                      , MI.postArchTermStmtAbsState = undefined
                      }
