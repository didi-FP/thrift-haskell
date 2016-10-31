{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE OverloadedStrings #-}

module Main where

import Control.Monad
import qualified Language.Thrift.Parser as T
import qualified Language.Thrift.AST as T
import System.Console.GetOpt (getOpt, usageInfo, ArgOrder(..), OptDescr(..), ArgDescr(..))
import System.Environment (getArgs)
import System.FilePath (takeBaseName, joinPath, addExtension)
import qualified Language.Haskell.Exts as H
import qualified Data.HashMap.Strict as HM
import qualified Data.Text as T
import Data.Text (Text)
import Data.List (find, foldl')
import Data.Char (toUpper)
import qualified Cases as C
import Data.List.Split (wordsBy)

--------------------------------------------------------------------------------

data CompileOpt = CompileOpt
        { cOptLensy :: Bool
        , cOptLazy :: Bool
        , cOptShowVersion :: Bool
        } deriving Show

defaultCompileOpt = CompileOpt
    { cOptLensy = False
    , cOptLazy  = False
    , cOptShowVersion = False
    }

main :: IO ()
main = do
    (cOpt, fs) <- getArgs >>= compilerOpts
    if cOptShowVersion cOpt
    then putStrLn "Version: thrift-haskell-compiler 0.1.0.0"
    else forM_ fs $ \ f -> do
        tAst <- T.parseFromFile f
        case tAst of Left err -> error (show err)
                     Right tAst -> do let (output, hAst) = compile f cOpt tAst
                                      putStrLn $ H.prettyPrintWithMode ppMode hAst
  where
    options :: [OptDescr (CompileOpt -> CompileOpt)]
    options =
        [ Option ['V','?'] ["version"]
            (NoArg (\ opts -> opts { cOptShowVersion = True }))
            "show version number"
        , Option ['l'] ["lens"]
            (NoArg (\ opts -> opts { cOptLensy = True}))
            "make lense for record fields"
        , Option ['z'] ["lazy"]
            (NoArg (\ opts -> opts { cOptLazy = True}))
            "use lazy fields in data declarations"
        ]
    compilerOpts :: [String] -> IO (CompileOpt, [String])
    compilerOpts argv = case getOpt Permute options argv of
        (o,n,[]  ) -> return (foldl (flip id) defaultCompileOpt o, n)
        (_,_,errs) -> ioError (userError (concat errs ++ usageInfo usage options))
    usage = "Usage: thrift-haskell-compiler [OPTION...] files..."
    ppMode = H.defaultMode

--------------------------------------------------------------------------------

-- | Find the module name
--
-- If the thrift doc contains namespace defs, use the last haskell def,
-- else use capitalized filename.
--
findNameSpace :: String -> [T.Header a] -> String
findNameSpace fn headers =
    let nss = (`concatMap` headers) $ \ h ->
            case h of T.HeaderNamespace (T.Namespace "haskell" ns _) -> [ns]
                      _                                              -> []
    in case nss of [] -> let a:bc = fn in toUpper a : bc
                   _  -> T.unpack (last nss)

compile :: FilePath -> CompileOpt -> T.Program a -> (FilePath, H.Module ())
compile fn cOpt@(CompileOpt lensy lazy _) (T.Program tHeaders tDefs) =
    ( addExtension (joinPath (wordsBy (== '.') ns )) "hs"
    , H.Module () (Just hModHead) hPragmas hImports hDecls
    )
  where
    ns = findNameSpace (takeBaseName fn) tHeaders
    hModHead = H.ModuleHead () (H.ModuleName () ns) Nothing Nothing

    hPragma s = H.LanguagePragma () [H.Ident () s]
    hImport mod =
        H.ImportDecl () (H.ModuleName () mod) False False False Nothing Nothing Nothing

    hPragmas = [
            hPragma "Test"
        ]
    hImports = [
            hImport "Data.Thrift.Test"
        ]

    hDecls = concatMap (mkDef cOpt) tDefs

--------------------------------------------------------------------------------

mkDef :: CompileOpt -> T.Definition a -> [H.Decl ()]
mkDef _ (T.ConstDefinition T.Const{..}) =
    [   H.TypeSig () [mkName constName] (mkType constValueType)
    ,   H.nameBind (mkName constName) (mkConstExp constValue)
    ]

mkDef _ (T.TypeDefinition (T.TypedefType T.Typedef{..})) =
    [   H.TypeDecl ()
            (H.DHead () (mkCapName typedefName))
            (mkType typedefTargetType)
    ]
mkDef _ (T.TypeDefinition (T.EnumType T.Enum{..})) =
    [   H.DataDecl () (H.DataType ()) Nothing
            (H.DHead () (mkCapName enumName))
            ((`map` enumValues) $ \ T.EnumDef{..} ->
                H.QualConDecl () Nothing Nothing
                    (H.ConDecl () (mkCapName enumDefName) [])
            )
            commonDeriving
    ,   H.InstDecl () Nothing (mkSimpleClassInst "Enum" (mkTypeConT enumName))
            (Just
                [   H.InsDecl () . H.FunBind () $ (`map` enums) $ \ (n, v) ->
                        H.Match () (H.name "fromEnum") [H.PApp () (unQual n) []]
                        (H.UnGuardedRhs () (H.intE v)) Nothing
                ,   H.InsDecl () . H.FunBind () $ (`map` enums) $ \ (n, v) ->
                        H.Match () (H.name "toEnum") [H.intP v]
                        (H.UnGuardedRhs () (H.Con () (unQual n))) Nothing
                ])
    ]
  where
    enums = zip (map (mkCapName .T.enumDefName) enumValues) enumDefValues
    enumDefValues = tail . reverse $ foldl' getValue [-1] (map T.enumDefValue enumValues)
    getValue acc@(x:xs) Nothing = (x + 1):acc
    getValue acc (Just x') = x' : acc

mkDef _ (T.TypeDefinition (T.StructType T.Struct{..})) =
    [   H.DataDecl () (H.DataType ()) Nothing
            (H.DHead () (mkCapName structName))
                [   H.QualConDecl () Nothing Nothing
                        (H.RecDecl () (mkCapName structName)
                        ((`map` structFields) $ \ T.Field{..} ->
                            H.FieldDecl () [mkName $ T.concat [structName, "_", fieldName]]
                                (mkFieldType fieldRequiredness fieldValueType)
                        ))
                ]
                commonDeriving
    ]

mkDef _ (T.TypeDefinition (T.SenumType T.Senum{..})) = error "mkDef: senum is deprecated"

mkDef _ (T.ServiceDefinition T.Service{..}) =
    (`concatMap` serviceFunctions) $ \ T.Function{..} ->
        let n = T.concat [serviceName, "_", functionName]
            reqName = mkCapName (n `T.append` "Req")
            resName = mkCapName (n `T.append` "Res")
            fname = mkName n
            returnType = maybe unitType mkType functionReturnType -- use unitType as thrift void
        in [
            H.DataDecl () (H.DataType ()) Nothing
                (H.DHead () reqName)
                    [   H.QualConDecl () Nothing Nothing
                            (H.RecDecl () resName
                            ((`map` functionParameters) $ \ T.Field{..} ->
                                H.FieldDecl () [mkName $ T.concat [n, "_", fieldName]]
                                    (mkFieldType fieldRequiredness fieldValueType)
                            ))
                    ]
                commonDeriving

        ,   case functionExceptions of
                Nothing ->
                    H.DataDecl () (H.NewType ()) Nothing
                        (H.DHead () resName)
                            [   H.QualConDecl () Nothing Nothing
                                    (H.ConDecl () resName [returnType])
                            ]
                    commonDeriving
                Just es ->
                    let returnTuples = (resName, returnType) :
                            ((`map` es) $ \ T.Field{..} ->
                                (mkName $ T.concat [n, "_", fieldName], mkType fieldValueType)
                            )
                    in H.DataDecl () (H.DataType ()) Nothing
                        (H.DHead () resName)
                            ((`map` returnTuples) $ \ (name, typ) ->
                                H.QualConDecl () Nothing Nothing (H.ConDecl () name [typ])
                            )
                        commonDeriving

        ,   H.TypeSig () [fname] $
                H.TyFun ()
                    (H.TyCon () (unQual reqName))
                    (H.TyApp () ioType (H.TyCon () (unQual resName)))
        ,   H.nameBind fname (H.Do () [])
        ]

--------------------------------------------------------------------------------

unQual :: H.Name () -> H.QName ()
unQual = H.UnQual ()

-- | translate thrift identifier into camelCased
mkName, mkCapName :: Text -> H.Name ()
mkName = H.Ident () . T.unpack .  C.process C.lower C.camel
mkCapName n = let (a, bc) = T.splitAt 1 . C.process C.lower C.camel $ n
              in H.Ident () . T.unpack $ T.toUpper a `T.append` bc

mkTypeConT :: Text -> H.Type ()
mkTypeConT t = H.TyCon () . unQual . mkCapName $ t

typCon :: String -> H.Type ()
typCon t = H.TyCon () . unQual . H.name $ t

mkFieldType :: Maybe T.FieldRequiredness -> T.TypeReference a -> H.Type ()
mkFieldType (Just T.Optional) t = H.TyApp () (typCon "Maybe") (mkType t)
mkFieldType _ t = mkType t

unitType :: H.Type ()
unitType = typCon "()"

ioType :: H.Type ()
ioType = typCon "IO"

mkDerivingInst :: String -> H.InstRule ()
mkDerivingInst x = H.IRule () Nothing Nothing (H.IHCon () (unQual (H.name x)))

commonDeriving :: Maybe (H.Deriving ())
commonDeriving = Just (H.Deriving () [
                    mkDerivingInst "Eq"
                ,   mkDerivingInst "Ord"
                ,   mkDerivingInst "Show"
                ])

mkSimpleClassInst :: String -> H.Type () -> H.InstRule ()
mkSimpleClassInst x t = H.IRule () Nothing Nothing
    (H.IHApp () (H.IHCon () (unQual (H.name x))) t)

mkType :: T.TypeReference a -> H.Type ()
mkType (T.DefinedType t _  ) = mkTypeConT t
mkType (T.StringType _ _   ) = typCon "T.Text"
mkType (T.BinaryType _ _   ) = typCon "B.ByteString"
mkType (T.SListType _ _    ) = error "mkType: slist is deprecated"
mkType (T.BoolType _ _     ) = typCon "Bool"
mkType (T.ByteType _ _     ) = typCon "Word.Word8"
mkType (T.I16Type _ _      ) = typCon "Int.Int16"
mkType (T.I32Type _ _      ) = typCon "Int.Int32"
mkType (T.I64Type _ _      ) = typCon "Int.Int64"
mkType (T.DoubleType _ _   ) = typCon "Double"
mkType (T.MapType kt vt _ _) = H.TyApp ()
                                    (H.TyApp () (typCon "HM.HashMap") (mkType kt))
                                    (mkType vt)
mkType (T.SetType vt _ _   ) = (H.TyApp () (typCon "HS.HashSet") (mkType vt))
mkType (T.ListType vt _ _  ) = H.TyList () (mkType vt)

mkConstExp :: T.ConstValue a -> H.Exp ()
mkConstExp (T.ConstInt i _       ) = H.intE i
mkConstExp (T.ConstFloat d _     ) = H.Lit () (H.Frac () (realToFrac d) (show d))
mkConstExp (T.ConstLiteral t _   ) = H.strE (T.unpack t)
mkConstExp (T.ConstIdentifier i _) = H.var (mkName i)
mkConstExp (T.ConstList cs _     ) = H.listE (map mkConstExp cs)
mkConstExp (T.ConstMap kvs _     ) = H.metaFunction "HM.fromList" [H.listE (map mkKV kvs)]
  where
    mkKV :: (T.ConstValue a, T.ConstValue b) -> H.Exp ()
    mkKV (k, v) = H.tuple [mkConstExp k, mkConstExp v]

--------------------------------------------------------------------------------
