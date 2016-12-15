{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE LambdaCase #-}

module Main where

import Control.Monad
import qualified Language.Thrift.Parser as T
import qualified Language.Thrift.AST as T
import System.Console.GetOpt (getOpt, usageInfo, ArgOrder(..), OptDescr(..), ArgDescr(..))
import System.Environment (getArgs)
import System.FilePath
import System.Directory
import qualified Language.Haskell.Exts as H
import qualified Data.IntMap.Strict as IM
import qualified Data.Text as T
import Data.Text (Text)
import Data.List (find, foldl', intercalate)
import Data.Char (toUpper)
import Text.Casing (pascal, camel)
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
    (opt, ps) <- getArgs >>= compilerOpts
    if cOptShowVersion opt
    then putStrLn "Version: thrift-haskell-compiler 0.1.0.0"
    else do
        cwd <- getCurrentDirectory
        compileAll cwd opt ps
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
    usage = "Usage: thrift-haskell-compiler [OPTION...] files/dirs..."

    compileAll :: FilePath -> CompileOpt -> [FilePath] -> IO ()
    compileAll cwd opt ps =
        forM_ ps $ \ p -> do
            isFile <- doesFileExist p
            if isFile
            then compile cwd opt p
            else compileAll cwd opt =<< listDirectory p

--------------------------------------------------------------------------------

compile :: FilePath     -- root directory
        -> CompileOpt   -- compile options
        -> FilePath     -- IDL file
        -> IO ()        -- compile and save
compile root opt p = do
    let p' = normalise $ root </> p
        relPath = makeRelative root p'

    T.parseFromFile relPath >>= \ case
        Left e -> do putStrLn ("parse " ++ relPath ++ " failed with: " ++ show e)
        Right (T.Program headers defs) -> do
            let moduleSpec = mkModuleSpec relPath
                imports = findImports (msPrefix moduleSpec) headers
                dels = compileDefs defs
                moduleHead = H.ModuleHead ()
                            (toModuleName moduleSpec)
                            Nothing
                            Nothing

                module_ = H.Module () (Just moduleHead)
                        defaultPragmas
                        (defaultImports ++ map toImportDel imports)
                        dels

            putStrLn $ H.prettyPrintWithMode H.defaultMode module_
  where
    -- Find all imports from thrift include
    findImports :: [String] -> [T.Header a] -> [ModuleSpec]
    findImports prefix = foldr go []
      where
        go x acc = case x of
            T.HeaderInclude (T.Include p _) -> mkImportModuleSpec prefix p:acc
            _                               -> acc

    -- Compile all thrift definitions into a export list and a declaration list.
    compileDefs :: [T.Definition a] -> [H.Decl ()]
    compileDefs = concatMap (compileTDef opt)

    -- Default enabled pragmas
    defaultPragmas :: [H.ModulePragma ()]
    defaultPragmas = [ hPragma "RecordWildCards"
              ]

    hPragma s = H.LanguagePragma () [H.Ident () s]

    -- Default imports
    defaultImports :: [H.ImportDecl ()]
    defaultImports = [ hImport "Data.Thrift" "Thrift"
                     ]

    hImport mod alias =
        H.ImportDecl () (H.ModuleName () mod) True False False Nothing
                     (Just (H.ModuleName () alias)) Nothing


--------------------------------------------------------------------------------

data ModuleSpec = ModuleSpec
    { msPrefix :: [String] -- ^ the prefix of the module name
    , msName :: String     -- ^ the last part of module name, we also use it as import alias
    }

-- | Join ModuleSpec into a module name string.
toImportDel :: ModuleSpec -> H.ImportDecl ()
toImportDel ms@(ModuleSpec prefix name) =
    H.ImportDecl () (toModuleName ms) True False False Nothing
                 (Just $ H.ModuleName () name) Nothing

-- | Join ModuleSpec into a module name string.
toModuleName :: ModuleSpec -> H.ModuleName ()
toModuleName (ModuleSpec prefix name) =
    H.ModuleName () $ concatMap (++ ".") prefix ++ name

-- | Make moduleName from a relative 'FilePath'.
mkModuleSpec :: FilePath -> ModuleSpec
mkModuleSpec fp = let ps = splitDirectories (dropExtension fp)
                  in ModuleSpec (pascal <$> init ps) (pascal $ last ps)


-- | Make moduleName from a include path with current prefix.
mkImportModuleSpec :: [String] -> Text -> ModuleSpec
mkImportModuleSpec prefix fp = let ps = splitDirectories (dropExtension (T.unpack fp))
                                   prefix' = pascal <$> init ps
                                   name = pascal $ last ps
                               in ModuleSpec (prefix ++ prefix') name


--------------------------------------------------------------------------------

compileTDef :: CompileOpt -> T.Definition a -> [H.Decl ()]
compileTDef _ (T.ConstDefinition T.Const{..}) =
    [   H.TypeSig () [mkName constName] (mkType constValueType)
    ,   H.nameBind (mkName constName) (mkConstExp constValue)
    ]

compileTDef _ (T.TypeDefinition (T.TypedefType T.Typedef{..})) =
    [   H.TypeDecl ()
            (H.DHead () (mkCapName typedefName))
            (mkType typedefTargetType)
    ]
compileTDef _ (T.TypeDefinition (T.EnumType T.Enum{..})) =
    [ H.DataDecl () (H.DataType ()) Nothing
        (H.DHead () (mkCapName enumName))
        ((`map` enumValues) $ \ T.EnumDef{..} ->
            H.QualConDecl () Nothing Nothing
                (H.ConDecl () (mkCapName enumDefName) [])
        )
        commonDeriving
    , H.InstDecl () Nothing (mkSimpleClassInst "Enum" (mkTypeConT enumName))
        (Just
            [ H.InsDecl () . H.FunBind () $ (`map` enums) $ \ (n, v) ->
                H.Match () (H.name "fromEnum") [H.PApp () (unQual n) []]
                (H.UnGuardedRhs () (H.intE v)) Nothing
            , H.InsDecl () . H.FunBind () $ (`map` enums) $ \ (n, v) ->
                H.Match () (H.name "toEnum") [H.intP v]
                (H.UnGuardedRhs () (H.Con () (unQual n))) Nothing
            ])
    , H.InstDecl () Nothing (mkSimpleClassInst "Thrift.Default" (mkTypeConT enumName))
        (Just
            [ H.InsDecl () . H.FunBind () $
                [ H.Match () (H.name "Thrift.def") []
                    (H.UnGuardedRhs () (H.var . mkCapName . T.enumDefName . head $ enumValues))
                    Nothing
                ]
            ])
    , H.InstDecl () Nothing (mkSimpleClassInst "Thrift.Thrift" (mkTypeConT enumName))
        (Just
            [ H.InsDecl () . H.FunBind () $ (`map` enums) $ \ (n, v) ->
                toTValueMatch
                    [H.PApp () (unQual n) []]
                    (H.UnGuardedRhs () $ tInt32Exp v) Nothing
            , H.InsDecl () . H.FunBind () $ ((`map` enums) $ \ (n, v) ->
                fromTValueMatch
                    [tInt32Pat v]   -- pattern match enum
                    (H.UnGuardedRhs ()
                        (H.app (H.var $ H.name "Right") $ H.Con () (unQual n)))
                        Nothing
                ) ++ [ fromTValueMatch
                        [H.wildcard]   -- pattern match enum
                        (H.UnGuardedRhs ()
                            (H.app (H.var $ H.name "Left") $ H.strE "bad enum value"))
                            Nothing
                ]
            ])
    ]
  where
    enums = zip (map (mkCapName .T.enumDefName) enumValues) enumDefValues
    enumDefValues = tail . reverse $ foldl' getValue [-1] (map T.enumDefValue enumValues)
    getValue acc@(x:xs) Nothing = (x + 1):acc
    getValue acc (Just x') = x' : acc

compileTDef _ (T.TypeDefinition (T.StructType T.Struct{..})) = case structFields of
    -- use '()' as empty request
    []      -> [ H.TypeDecl () (H.DHead () sName) unitType ]
    -- use record for a non empty struct
    sFields ->
        [ H.DataDecl () (H.DataType ()) Nothing
            (H.DHead () sName)
                [ H.QualConDecl () Nothing Nothing . H.RecDecl () sName $
                    (`map` sFields) $ \ T.Field{..} ->
                        H.FieldDecl () [mkName $ T.concat [structName, "_", fieldName]]
                            (mkFieldType fieldRequiredness fieldValueType)
                ]
                commonDeriving
        , H.InstDecl () Nothing (mkSimpleClassInst "Thrift.Default" (mkTypeConT structName))
            (Just
                [ H.InsDecl () $ H.sfun (H.name "Thrift.def") []
                        (H.UnGuardedRhs () $ H.letE ((`map` sFields) $ \ T.Field{..} ->
                            H.patBind
                                (H.pvar . mkName $ T.concat [structName, "_", fieldName])
                                (case fieldDefaultValue of
                                    Nothing -> H.var . H.name $ "Thrift.def"
                                    Just d  -> case fieldRequiredness of
                                        Just T.Optional ->
                                            H.metaFunction "Just" [mkConstExp d]
                                        _ -> mkConstExp d
                                )
                            )
                            sExp
                        )
                        Nothing
                ])
        , H.InstDecl () Nothing (mkSimpleClassInst "Thrift.Thrift" (mkTypeConT structName))
            (Just
                [ H.InsDecl () . H.FunBind () $
                    [ toTValueMatch
                        [sPat]
                        (H.UnGuardedRhs () . tStructExp . H.List () $
                            (`map` sFields) $ \ T.Field{..} ->
                                let n = camel . T.unpack . T.concat $ [structName, "_", fieldName]
                                in case fieldIdentifier of
                                    Nothing -> error $ "can't find field identifier for " ++ n
                                    Just fid ->
                                        H.Tuple () H.Boxed
                                            [ H.intE fid
                                            , toTValueFun (H.var $ H.name n)
                                            ]
                        )
                        Nothing
                    ]
                , H.InsDecl () . H.FunBind () $
                    let mBind = H.nameBind $ H.name "m"
                        mExp = H.var $ H.name "m"
                        xPat = H.pvar $ H.name "x"
                        xExp = H.var $ H.name "x"
                    -- convert to 'IM.IntMap' first, then use 'Maybe' monad
                    in [ fromTValueMatch
                        [tStructPat xPat]   -- pattern match enum
                        (H.UnGuardedRhs () . H.doE $
                            (H.letStmt
                                [mBind (H.metaFunction "IM.fromList" [xExp])]
                            ) : ((`map` sFields) $ \ T.Field{..} ->
                                let n = camel . T.unpack . T.concat $ [structName, "_", fieldName]
                                    -- how 'T.FieldRequiredness' affect deserialization
                                    lookupFunc = case (fieldRequiredness, fieldDefaultValue) of
                                        -- default field
                                        (Nothing, Nothing) ->
                                            H.metaFunction "Thrift.lookupDefault" .
                                                (H.var (H.name "Thrift.def") :)
                                        (Nothing, Just d) ->
                                            H.metaFunction "Thrift.lookupDefault" .
                                                (mkConstExp d :)
                                        (Just T.Optional, Nothing) ->
                                            H.metaFunction "Thrift.lookupOptional" .
                                                (H.var (H.name "Nothing") :)
                                        (Just T.Optional, Just d) ->
                                            H.metaFunction "Thrift.lookupOptional" .
                                                (H.metaFunction "Just" [mkConstExp d] :)
                                        (Just T.Required, _) ->
                                            H.metaFunction "Thrift.lookupRequired"

                                in case fieldIdentifier of
                                    Nothing -> error $ "can't find field identifier for " ++ n
                                    Just fid ->
                                        H.genStmt (H.pvar $ H.name n)
                                            (lookupFunc
                                                [ H.metaFunction "fromIntegral" [H.intE fid]
                                                , mExp
                                                ])
                            ) ++ [H.qualStmt (H.metaFunction "return" [sExp])]
                        )
                        Nothing
                    , fromTValueMatch
                        [H.wildcard]   -- pattern match enum
                        (H.UnGuardedRhs ()
                            (H.app leftCon $ H.strE "bad struct value"))
                        Nothing
                    ]
                ])
        ]
  where
    sName = mkCapName structName
    -- we rely on @RecordWildCards@ to simplify naming
    sPat = H.PRec () (unQual sName) [H.PFieldWildcard ()]
    sExp = H.RecConstr () (unQual sName) [H.FieldWildcard ()]
    fieldIdentifier = tail . reverse $ foldl' getValue [-1] (map T.fieldIdentifier structFields)
    getValue acc@(x:xs) Nothing = (x + 1):acc
    getValue acc (Just x') = x' : acc

compileTDef _ (T.TypeDefinition (T.SenumType T.Senum{..})) = error "compileTDef: senum is deprecated"

compileTDef cOpt (T.ServiceDefinition T.Service{..}) =
    (`concatMap` serviceFunctions) $ \ T.Function{..} ->
        let n = T.concat [serviceName, "_", functionName]
            reqName = mkCapName (n `T.append` "Req")
            resName = mkCapName (n `T.append` "Res")
            fname = mkName n
            -- use unitType as thrift void
            returnType = maybe unitType mkType functionReturnType
        in concat $ [
            -- use struct to present request
            case functionParameters of
                _  ->
                    let reqStruct = T.Struct {
                            T.structName = n `T.append` "Req"
                        ,   T.structFields = functionParameters
                        ,   T.structAnnotations = []
                        ,   T.structDocstring = functionDocstring  -- unused
                        ,   T.structSrcAnnot = functionSrcAnnot    -- unused
                        ,   T.structKind = T.StructKind            -- unused
                        }
                    in compileTDef cOpt (T.TypeDefinition (T.StructType reqStruct))

            -- use struct to present respond
        ,   let resFields = case functionReturnType of
                    Nothing      ->  []
                    Just retType -> [
                        T.Field {   -- return value is taken as field 0
                                    T.fieldIdentifier = Just 0
                                    -- return value is taken as default field
                                ,   T.fieldRequiredness = Nothing
                                ,   T.fieldValueType = retType
                                    -- no field name to void collision with exception fields
                                ,   T.fieldName = ""
                                    -- return value doesn't have default value
                                ,   T.fieldDefaultValue = Nothing
                                ,   T.fieldAnnotations = [] -- unused
                                ,   T.fieldDocstring = functionDocstring -- unused
                                ,   T.fieldSrcAnnot = functionSrcAnnot -- unused
                        }]

                expFields = case functionExceptions of Nothing -> []
                                                       Just es -> es

                resStruct = T.Struct {
                            T.structName = n `T.append` "Res"
                        ,   T.structFields = resFields ++ expFields
                        ,   T.structAnnotations = []
                        ,   T.structDocstring = functionDocstring      -- unused
                        ,   T.structSrcAnnot = functionSrcAnnot        -- unused
                        ,   T.structKind = T.StructKind                -- unused
                        }
            in compileTDef cOpt (T.TypeDefinition (T.StructType resStruct))

        ,   [ H.TypeDecl () (H.DHead () (mkCapName n)) $
                H.TyFun ()
                    (H.TyCon () (unQual reqName))
                    (H.TyApp () ioType (H.TyCon () (unQual resName)))
            ]
        ]

--------------------------------------------------------------------------------

-- | Directly make a 'QName' from 'Name', we never use constructor directly
-- because we just use "X.Y" style 'String'.
unQual :: H.Name () -> H.QName ()
unQual = H.UnQual ()

-- | translate thrift identifier into camelCased
mkName, mkCapName :: Text -> H.Name ()
mkName = H.Ident () . camel . T.unpack
mkCapName = H.Ident () . pascal . T.unpack

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
mkType (T.StringType _ _   ) = typCon "Thrift.Text"
mkType (T.BinaryType _ _   ) = typCon "Thrift.ByteString"
mkType (T.SListType _ _    ) = error "mkType: slist is deprecated"
mkType (T.BoolType _ _     ) = typCon "Bool"
mkType (T.ByteType _ _     ) = typCon "Thrift.Word8"
mkType (T.I16Type _ _      ) = typCon "Thrift.Int16"
mkType (T.I32Type _ _      ) = typCon "Thrift.Int32"
mkType (T.I64Type _ _      ) = typCon "Thrift.Int64"
mkType (T.DoubleType _ _   ) = typCon "Double"
mkType (T.MapType kt vt _ _) = H.TyApp ()
                                    (H.TyList () (H.TyTuple () H.Unboxed [mkType kt, mkType vt]))
                                    (mkType vt)
mkType (T.SetType vt _ _   ) = H.TyList () (mkType vt)
mkType (T.ListType vt _ _  ) = H.TyList () (mkType vt)

mkConstExp :: T.ConstValue a -> H.Exp ()
mkConstExp (T.ConstInt i _       ) = H.intE i
mkConstExp (T.ConstFloat d _     ) = H.Lit () (H.Frac () (realToFrac d) (show d))
mkConstExp (T.ConstLiteral t _   ) = H.strE (T.unpack t)
mkConstExp (T.ConstIdentifier i _) = let is = map (pascal . T.unpack) $ T.split (== '.') i
                                         i' = case is of
                                            [datatyp, constr] -> constr
                                            [imp, _, constr]  -> imp ++ "." ++ constr
                                            _ -> error $ "bad const identifier: " ++ T.unpack i
                                     in H.var (H.name i')
mkConstExp (T.ConstList cs _     ) = H.listE (map mkConstExp cs)
mkConstExp (T.ConstMap kvs _     ) = H.listE (map mkKV kvs)
  where
    mkKV :: (T.ConstValue a, T.ConstValue b) -> H.Exp ()
    mkKV (k, v) = H.tuple [mkConstExp k, mkConstExp v]

--------------------------------------------------------------------------------

tInt32Exp :: Integer -> H.Exp ()
tInt32Exp x = H.App () (H.Con () . unQual $ H.name "Thrift.TInt32") (H.intE x)

tInt32Pat :: Integer -> H.Pat ()
tInt32Pat x = H.PApp () (unQual $ H.name "Thrift.TInt32") [H.intP x]

tStructExp :: H.Exp () -> H.Exp ()
tStructExp x =  H.App () (H.Con () . unQual $ H.name "Thrift.TStruct") x

tStructPat :: H.Pat () -> H.Pat ()
tStructPat x = H.PApp () (unQual $ H.name "Thrift.TStruct") [x]

fromTValueFun :: H.Exp () -> H.Exp ()
fromTValueFun x = H.metaFunction "Thrift.fromTValue" [x]

fromTValueMatch :: [H.Pat ()] -> (H.Rhs ()) -> Maybe (H.Binds ()) -> H.Match ()
fromTValueMatch = H.Match () (H.name "Thrift.fromTValue")

toTValueFun :: H.Exp () -> H.Exp ()
toTValueFun x = H.metaFunction "Thrift.toTValue" [x]

toTValueMatch :: [H.Pat ()] -> (H.Rhs ()) -> Maybe (H.Binds ()) -> H.Match ()
toTValueMatch = H.Match () (H.name "Thrift.toTValue")

leftCon :: H.Exp ()
leftCon = H.Con () . unQual $ H.name "Left"

rightCon :: H.Exp ()
rightCon = H.Con () . unQual $ H.name "Right"

--------------------------------------------------------------------------------
