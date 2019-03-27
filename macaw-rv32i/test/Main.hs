{-# LANGUAGE DataKinds #-}

module Main where

import Control.Exception
import qualified Data.ByteString as B
import qualified Data.ElfEdit as E
import Data.Macaw.Discovery
import Data.Macaw.Memory
import Data.Macaw.Memory.ElfLoader
import Data.Macaw.RV32I
import Data.Map as M

withELF :: FilePath -> (E.Elf 32 -> IO ()) -> IO ()
withELF fp k = do
  bytes <- B.readFile fp
  case E.parseElf bytes of
    E.ElfHeaderError off msg ->
      error ("Error parsing ELF header at offset " ++ show off ++ ": " ++ msg)
    E.Elf32Res [] e32 -> k e32
    E.Elf64Res [] e64 -> error "ELF64 is unsupported in the test suite"
    E.Elf32Res errs _ -> error ("Errors while parsing ELF file: " ++ show errs)
    E.Elf64Res errs _ -> error ("Errors while parsing ELF file: " ++ show errs)

main = withELF "test/tests/I-ADD-01.elf" $ \e -> do
  (warnings, memory, offset, symbols) <- case resolveElfContents defaultLoadOptions e of
    Left e -> ioError (userError "couldn't resolve elf file")
    Right (_, _, Nothing, _) -> ioError (userError "couldn't resolve entry point")
    Right (warning, memory, Just offset, symbols) -> return (warning, memory, offset, symbols)
  let addrSymMap :: M.Map (MemSegmentOff 32) B.ByteString
      addrSymMap = M.fromList [ (memSymbolStart sym, memSymbolName sym)
                              | sym <- symbols
                              ]
  let di = cfgFromAddrs rv32i_linux_info memory addrSymMap [offset] []
  print $ ppDiscoveryStateBlocks di

