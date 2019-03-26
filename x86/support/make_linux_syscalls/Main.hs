{-
The driver for creating SyscallInfo.Linux
-}
{-# LANGUAGE ViewPatterns #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE PatternGuards #-}
module Main (main) where

import           Data.Attoparsec.ByteString.Char8 (Parser)
import qualified Data.Attoparsec.ByteString.Char8 as P
import           Data.ByteString (ByteString)
import qualified Data.ByteString.Char8 as BS
import           Data.Either (either)
import           Data.Map (Map)
import qualified Data.Map as Map
import           Data.Maybe (catMaybes)
import           Language.C
import           Language.C.Analysis.AstAnalysis
import           Language.C.Analysis.SemRep
import           Language.C.Analysis.TravMonad
import           Language.C.Data.Ident
import           Language.C.System.GCC (newGCC)
import           System.Environment (getArgs)
import           System.Exit
import           System.IO
import           Text.PrettyPrint
import           Text.Show.Pretty

-- FIXME: clag from Data.Macaw.Architecture.Syscall
data SyscallArgType = VoidArgType | WordArgType | XMMFloatType
                    deriving (Eq, Show, Read)


-- | The syscall_64.tbl file looks like (after pre-processing)
--
-- # comment1
-- ... more comments
-- # commentn
-- <syscall no.>\t+<abi>\t+<syscall name>\t<entry point>
--
-- for example
--
-- 5       common  fstat                   sys_newfstat
--
-- entry point is optional -- if it is not present, syscall name is used.

data SyscallInfo =
  SyscallInfo { syscallNo    :: Integer
              , _syscallABI   :: ByteString
              , syscallName  :: ByteString
              , syscallProto :: Either ByteString IdentDecl
              }
  -- deriving Show

instance Show SyscallInfo where
  show si = show $ integer (syscallNo si) <+> text (show $ syscallName si)
                 <+> text "->"
            <+> either (const "???") pretty (syscallProto si)

-- A bit hacky, but no less than using TH to read in a text file
generateHSFile :: [SyscallInfo] -> Doc
generateHSFile sis =
  vcat $ [ text "-- DO NOT EDIT.  Generated from make_linux_syscalls/Main.hs"
         , text "module Data.Macaw.X86.SyscallInfo.Linux (syscallInfo) where"
         , text "import           Data.Map (Map, fromList)"
         , text "import           Data.Word"
         , text "import           Data.Macaw.X86.SyscallInfo"
         , text ""
         , text "syscallInfo :: Map Word64 SyscallTypeInfo"
         , text "syscallInfo =" <+> ppDoc syscallMap ]
         ++ unknownSyscalls
  where
    syscallMap = Map.fromList [ (syscallNo si, syscallInfo idecl)
                              | si <- sis
                              , Right idecl <- [ syscallProto si ] ]
    unknownSyscalls =
      text "-- Unknown system calls: " :
      [ text "--" <+> integer (syscallNo si) <+> text (BS.unpack str)
      | si <- sis
      , Left str <- [ syscallProto si ]  ]

    -- syscallInfo :: CExtDecl -> Maybe (String, SyscallArgType, [SyscallArgType])
    syscallInfo d =
      case getVarDecl d of
        VarDecl vname _ (FunctionType (FunType rettyp params _) _) ->
          ( identToString (identOfVarName vname)
          , typeToArgType rettyp
          , map (typeToArgType . declType) params )
        _ -> error "syscallInfo given unexpected declaration."

    -- syscallInfo (CDeclExt (CDecl [CTypeSpec spec] [(Just declr, _, _)] _))
    --   | CDeclr (Just ident) [CFunDeclr (Right (decls, _)) _ _] _ _ _ <- declr
    --   = Just (identToString ident, typeSpecToArgType spec, map declToArgType decls)
    -- syscallInfo d = error ("unhandled decl" ++ show d)

    -- declToArgType (CDecl [CTypeSpec spec]  _) = typeSpecToArgType spec
    -- declToArgType d = error ("unhandled decl (in type) " ++ show d)

typeToArgType :: Type -> SyscallArgType
typeToArgType tp =
  case tp of
    DirectType typ' _ _ ->
      case typ' of
        TyVoid               -> VoidArgType
        TyIntegral _         -> WordArgType
        TyFloating TyLDouble -> unhandled
        TyFloating _         -> XMMFloatType
        TyComplex _          -> unhandled
        TyComp _comp         -> unhandled -- compTypeToArgType comp
        TyEnum _             -> WordArgType -- FIXME: ???
        TyBuiltin _          -> unhandled
    PtrType _ _ _            -> WordArgType
    ArrayType _ _ _ _        -> WordArgType
    FunctionType _ _         -> unhandled
    TypeDefType (TypeDefRef _ typ _) _ _ -> typeToArgType typ
  where
    unhandled = error ("Unhandled type: " ++ show (pretty tp))

-- We support as little as possible here ...
-- typeSpecToArgType :: Show a => CTypeSpecifier a -> SyscallArgType
-- typeSpecToArgType tspec =
--   case tspec of
--     CVoidType _   -> VoidArgType
--     CCharType _   -> WordArgType
--     CShortType _  -> WordArgType
--     CIntType _    -> WordArgType
--     CLongType _   -> WordArgType
--     CFloatType _  -> XMMFloatType
--     CDoubleType _ -> XMMFloatType
--     CSignedType _ -> WordArgType
--     CUnsigType _  -> WordArgType
--     CBoolType _   -> WordArgType
--     CSUType _ _ -> unhandled
--     CEnumType _ _ -> WordArgType -- FIXME: does it fit in a word?
--     CComplexType _ -> unhandled
--     CTypeDef __dent _ -> unhandled
--     CTypeOfExpr _ _ -> unhandled
--     CTypeOfType _ _ -> unhandled
--   where
--     unhandled = error ("Unhandled type specifier: " ++ show tspec)

readOneCFile :: FilePath -> IO (Map ByteString IdentDecl)
readOneCFile f = do
  r <- parseCFile (newGCC "gcc") Nothing [] f
  tunit <- case r of
             Right v  -> return v
             Left err -> error (show err)

  let gdecls = case runTrav_ (analyseAST tunit) of
                 Left err -> error $ "Error: " ++ (show err)
                 Right (d, _) -> d

  return (Map.mapKeys (\(Ident name _ _) -> BS.pack name) $ gObjs gdecls)

syscallLine :: Map ByteString IdentDecl -> Parser (Maybe SyscallInfo)
syscallLine idents = do
  P.skipSpace
  P.choice [ P.char '#' >> return Nothing
           , P.endOfInput >> return Nothing
           , parseLine
           ]
  where
    parseLine = do
      num   <- P.decimal
      P.skipSpace
      abi  <- P.takeWhile (not . P.isSpace)
      P.skipSpace
      name <- P.takeWhile (not . P.isSpace)
      P.skipSpace
      ident <- P.choice [ P.takeWhile1 (not . P.isSpace)
                        , return name ]
      if abi == "x32"
        then return Nothing
        else return (Just (SyscallInfo num abi name (findIdent ident)))

    findIdent bytes =
      case (Map.lookup bytes idents) of
        Nothing -> Left bytes
        Just v  -> Right v
      -- -- FIXME: we should maybe chain through newNameSupply?  I don't think it is ever used ...
      -- case execParser expressionP bytes (position 0 "" 0 0) idents newNameSupply of
      --   Right (CVar ident _, _unusedNames) -> Just ident
      --   Right (e, _)                       -> trace ("Unknown expr: " ++ show e) Nothing
      --   _                                  -> trace ("Couldn't find '" ++ show bytes ++ "'") Nothing


showUsageAndExit :: IO a
showUsageAndExit = do
  hPutStrLn stderr $ unlines
    [ "This program generates the Haskell module that maps system call ids"
    , "in Linux to the name, argument types, and result type."
    , ""
    , "Please specify the syscals_tbl file and header files arguments."
    , "The resulting Haskell module will be written to standard out."
    ]
  exitFailure


main :: IO ()
main = do
  args <- getArgs

  (syscalls_tbl_file, h_files) <-
    case args of
      tbl_file : h_files -> pure (tbl_file, h_files)
      [] -> showUsageAndExit

  syscalls <- BS.lines <$> BS.readFile syscalls_tbl_file
  gdecls <- mconcat <$> mapM readOneCFile h_files

  ms <- case mapM (P.parseOnly (syscallLine gdecls)) syscalls of
          Left err -> error $ "Error: " ++  err
          Right ms -> return ms

  -- mapM_ pp (catMaybes ms)
  print (generateHSFile $ catMaybes ms)
