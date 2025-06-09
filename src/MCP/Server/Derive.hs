{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TemplateHaskell   #-}

module MCP.Server.Derive
  ( -- * Template Haskell Derivation
    derivePromptHandler
  , derivePromptHandlerWithDescription
  , deriveResourceHandler
  , deriveResourceHandlerWithDescription
  , deriveToolHandler
  , deriveToolHandlerWithDescription
  ) where

import qualified Data.Map            as Map
import qualified Data.Text           as T
import           Language.Haskell.TH
import           Text.Read           (readMaybe)
import qualified Data.Char           as Char

import           MCP.Server.Types

-- Helper function to convert PascalCase/camelCase to snake_case
toSnakeCase :: String -> String
toSnakeCase [] = []
toSnakeCase (x:xs) = Char.toLower x : go xs
  where
    go [] = []
    go (c:cs)
      | Char.isUpper c = '_' : Char.toLower c : go cs
      | otherwise = c : go cs

-- Helper function to convert Clause to Match
clauseToMatch :: Clause -> Match
clauseToMatch (Clause ps b ds) = Match (case ps of [p] -> p; _ -> TupP ps) b ds

-- | Derive prompt handlers from a data type with custom descriptions
-- Usage: $(derivePromptHandlerWithDescription ''MyPrompt 'handlePrompt [("Constructor", "Description")])
derivePromptHandlerWithDescription :: Name -> Name -> [(String, String)] -> Q Exp
derivePromptHandlerWithDescription typeName handlerName descriptions = do
  info <- reify typeName
  case info of
    TyConI (DataD _ _ _ _ constructors _) -> do
      -- Generate prompt definitions
      promptDefs <- sequence $ map (mkPromptDefWithDescription descriptions) constructors

      -- Generate list handler
      listHandlerExp <- [| \_cursor -> pure $ PaginatedResult
        { paginatedItems = $(return $ ListE promptDefs)
        , paginatedNextCursor = Nothing
        } |]

      -- Generate get handler with cases
      cases <- sequence $ map (mkPromptCase handlerName) constructors
      defaultCase <- [| pure $ Left $ InvalidPromptName $ "Unknown prompt: " <> name |]
      let defaultMatch = Match WildP (NormalB defaultCase) []

      getHandlerExp <- return $ LamE [VarP (mkName "name"), VarP (mkName "args")] $
        CaseE (AppE (VarE 'T.unpack) (VarE (mkName "name")))
          (map clauseToMatch cases ++ [defaultMatch])

      return $ TupE [Just listHandlerExp, Just getHandlerExp]
    _ -> fail $ "derivePromptHandlerWithDescription: " ++ show typeName ++ " is not a data type"

-- | Derive prompt handlers from a data type
-- Usage: $(derivePromptHandler ''MyPrompt 'handlePrompt)
derivePromptHandler :: Name -> Name -> Q Exp
derivePromptHandler typeName handlerName = 
  derivePromptHandlerWithDescription typeName handlerName []

mkPromptDefWithDescription :: [(String, String)] -> Con -> Q Exp
mkPromptDefWithDescription descriptions con = 
  case con of
    NormalC name [] -> do
      let promptName = T.pack . toSnakeCase . nameBase $ name
      let constructorName = nameBase name
      let description = case lookup constructorName descriptions of
            Just desc -> desc
            Nothing   -> "Handle " ++ constructorName
      [| PromptDefinition
          { promptDefinitionName = $(litE $ stringL $ T.unpack promptName)
          , promptDefinitionDescription = $(litE $ stringL description)
          , promptDefinitionArguments = []
          } |]
    RecC name fields -> do
      let promptName = T.pack . toSnakeCase . nameBase $ name
      let constructorName = nameBase name
      let description = case lookup constructorName descriptions of
            Just desc -> desc
            Nothing   -> "Handle " ++ constructorName
      args <- sequence $ map (mkArgDef descriptions) fields
      [| PromptDefinition
          { promptDefinitionName = $(litE $ stringL $ T.unpack promptName)
          , promptDefinitionDescription = $(litE $ stringL description)
          , promptDefinitionArguments = $(return $ ListE args)
          } |]
    _ -> fail "Unsupported constructor type"


mkArgDef :: [(String, String)] -> (Name, Bang, Type) -> Q Exp
mkArgDef descriptions (fieldName, _, fieldType) = do
  let isOptional = case fieldType of
        AppT (ConT n) _ -> nameBase n == "Maybe"
        _               -> False
  let fieldNameStr = nameBase fieldName
  let description = case lookup fieldNameStr descriptions of
        Just desc -> desc
        Nothing   -> fieldNameStr
  [| ArgumentDefinition
      { argumentDefinitionName = $(litE $ stringL fieldNameStr)
      , argumentDefinitionDescription = $(litE $ stringL description)
      , argumentDefinitionRequired = $(if isOptional then [| False |] else [| True |])
      } |]

mkPromptCase :: Name -> Con -> Q Clause
mkPromptCase handlerName (NormalC name []) = do
  let promptName = T.pack . toSnakeCase . nameBase $ name
  clause [litP $ stringL $ T.unpack promptName]
    (normalB [| do
        content <- $(varE handlerName) $(conE name)
        pure $ Right content |])
    []
mkPromptCase handlerName (RecC name fields) = do
  let promptName = T.pack . toSnakeCase . nameBase $ name
  body <- mkRecordCase name handlerName fields
  clause [litP $ stringL $ T.unpack promptName] (normalB (return body)) []
mkPromptCase _ _ = fail "Unsupported constructor type"

mkRecordCase :: Name -> Name -> [(Name, Bang, Type)] -> Q Exp
mkRecordCase conName handlerName fields = do
  case fields of
    [] -> [| do
        content <- $(varE handlerName) $(conE conName)
        pure $ Right content |]
    _ -> do
      -- Build nested case expressions for field validation
      buildNestedFieldValidation conName handlerName fields 0

-- Build nested case expressions for field validation, supporting any number of fields
buildNestedFieldValidation :: Name -> Name -> [(Name, Bang, Type)] -> Int -> Q Exp
buildNestedFieldValidation conName handlerName [] depth = do
  -- Base case: all fields validated, build constructor application
  let fieldVars = [mkName ("field" ++ show i) | i <- [0..depth-1]]
  let constructorApp = foldl AppE (ConE conName) (map VarE fieldVars)
  [| do
      content <- $(varE handlerName) $(return constructorApp)
      pure $ Right content |]

buildNestedFieldValidation conName handlerName ((fieldName, _, fieldType):remainingFields) depth = do
  let fieldStr = nameBase fieldName
  let (isOptional, innerType) = case fieldType of
        AppT (ConT n) inner | nameBase n == "Maybe" -> (True, inner)
        other                                       -> (False, other)
  let fieldVar = mkName ("field" ++ show depth)

  continuation <- buildNestedFieldValidation conName handlerName remainingFields (depth + 1)

  -- Generate conversion expression based on type
  let convertExpr rawVar = case innerType of
        ConT n | nameBase n == "Int" ->
          [| case readMaybe (T.unpack $(varE rawVar)) of
               Just parsed -> parsed
               Nothing -> error $ "Failed to parse Int from: " <> T.unpack $(varE rawVar) |]
        ConT n | nameBase n == "Integer" ->
          [| case readMaybe (T.unpack $(varE rawVar)) of
               Just parsed -> parsed
               Nothing -> error $ "Failed to parse Integer from: " <> T.unpack $(varE rawVar) |]
        ConT n | nameBase n == "Double" ->
          [| case readMaybe (T.unpack $(varE rawVar)) of
               Just parsed -> parsed
               Nothing -> error $ "Failed to parse Double from: " <> T.unpack $(varE rawVar) |]
        ConT n | nameBase n == "Float" ->
          [| case readMaybe (T.unpack $(varE rawVar)) of
               Just parsed -> parsed
               Nothing -> error $ "Failed to parse Float from: " <> T.unpack $(varE rawVar) |]
        ConT n | nameBase n == "Bool" ->
          [| case T.toLower $(varE rawVar) of
               "true" -> True
               "false" -> False
               _ -> error $ "Failed to parse Bool from: " <> T.unpack $(varE rawVar) |]
        _ -> varE rawVar  -- Text or other types, use as-is

  if isOptional
    then do
      rawFieldVar <- newName ("raw" ++ show depth)
      convertedExpr <- convertExpr rawFieldVar
      [| do
          let $(varP fieldVar) = case Map.lookup $(litE $ stringL fieldStr) (Map.fromList args) of
                Nothing                  -> Nothing
                Just $(varP rawFieldVar) -> Just $(return convertedExpr)
          $(return continuation) |]
    else do
      rawFieldVar <- newName ("raw" ++ show depth)
      convertedExpr <- convertExpr rawFieldVar
      [| case Map.lookup $(litE $ stringL fieldStr) (Map.fromList args) of
          Just $(varP rawFieldVar) -> do
            let $(varP fieldVar) = $(return convertedExpr)
            $(return continuation)
          Nothing -> pure $ Left $ MissingRequiredParams $ "field '" <> $(litE $ stringL fieldStr) <> "' is missing" |]

-- | Derive resource handlers from a data type with custom descriptions
-- Usage: $(deriveResourceHandlerWithDescription ''MyResource 'handleResource [("Constructor", "Description")])
deriveResourceHandlerWithDescription :: Name -> Name -> [(String, String)] -> Q Exp
deriveResourceHandlerWithDescription typeName handlerName descriptions = do
  info <- reify typeName
  case info of
    TyConI (DataD _ _ _ _ constructors _) -> do
      -- Generate resource definitions
      resourceDefs <- sequence $ map (mkResourceDefWithDescription descriptions) constructors

      listHandlerExp <- [| \_cursor -> pure $ PaginatedResult
        { paginatedItems = $(return $ ListE resourceDefs)
        , paginatedNextCursor = Nothing
        } |]

      -- Generate read handler with cases
      cases <- sequence $ map (mkResourceCase handlerName) constructors
      defaultCase <- [| pure $ Left $ ResourceNotFound $ "Resource not found: " <> T.pack unknown |]
      let defaultMatch = Match (VarP (mkName "unknown")) (NormalB defaultCase) []

      readHandlerExp <- return $ LamE [VarP (mkName "uri")] $
        CaseE (AppE (VarE 'show) (VarE (mkName "uri")))
          (map clauseToMatch cases ++ [defaultMatch])

      return $ TupE [Just listHandlerExp, Just readHandlerExp]
    _ -> fail $ "deriveResourceHandlerWithDescription: " ++ show typeName ++ " is not a data type"

-- | Derive resource handlers from a data type
-- Usage: $(deriveResourceHandler ''MyResource 'handleResource)
deriveResourceHandler :: Name -> Name -> Q Exp
deriveResourceHandler typeName handlerName = 
  deriveResourceHandlerWithDescription typeName handlerName []

mkResourceDefWithDescription :: [(String, String)] -> Con -> Q Exp
mkResourceDefWithDescription descriptions (NormalC name []) = do
  let resourceName = T.pack . toSnakeCase . nameBase $ name
  let resourceURI = "resource://" <> T.unpack resourceName
  let constructorName = nameBase name
  let description = case lookup constructorName descriptions of
        Just desc -> Just desc
        Nothing   -> Just constructorName
  [| ResourceDefinition
      { resourceDefinitionURI = $(litE $ stringL resourceURI)
      , resourceDefinitionName = $(litE $ stringL $ T.unpack resourceName)
      , resourceDefinitionDescription = $(case description of
          Just desc -> [| Just $(litE $ stringL desc) |]
          Nothing   -> [| Nothing |])
      , resourceDefinitionMimeType = Just "text/plain"
      } |]
mkResourceDefWithDescription _ _ = fail "Unsupported constructor type for resources"


mkResourceCase :: Name -> Con -> Q Clause
mkResourceCase handlerName (NormalC name []) = do
  let resourceName = T.pack . toSnakeCase . nameBase $ name
  let resourceURI = "resource://" <> T.unpack resourceName
  clause [litP $ stringL resourceURI]
    (normalB [| Right <$> $(varE handlerName) $(conE name) |])
    []
mkResourceCase _ _ = fail "Unsupported constructor type for resources"

-- | Derive tool handlers from a data type with custom descriptions
-- Usage: $(deriveToolHandlerWithDescription ''MyTool 'handleTool [("Constructor", "Description")])
deriveToolHandlerWithDescription :: Name -> Name -> [(String, String)] -> Q Exp
deriveToolHandlerWithDescription typeName handlerName descriptions = do
  info <- reify typeName
  case info of
    TyConI (DataD _ _ _ _ constructors _) -> do
      -- Generate tool definitions
      toolDefs <- sequence $ map (mkToolDefWithDescription descriptions) constructors

      listHandlerExp <- [| \_cursor -> pure $ PaginatedResult
        { paginatedItems = $(return $ ListE toolDefs)
        , paginatedNextCursor = Nothing
        } |]

      -- Generate call handler with cases
      cases <- sequence $ map (mkToolCase handlerName) constructors
      defaultCase <- [| pure $ Left $ UnknownTool $ "Unknown tool: " <> name |]
      let defaultMatch = Match WildP (NormalB defaultCase) []

      callHandlerExp <- return $ LamE [VarP (mkName "name"), VarP (mkName "args")] $
        CaseE (AppE (VarE 'T.unpack) (VarE (mkName "name")))
          (map clauseToMatch cases ++ [defaultMatch])

      return $ TupE [Just listHandlerExp, Just callHandlerExp]
    _ -> fail $ "deriveToolHandlerWithDescription: " ++ show typeName ++ " is not a data type"

-- | Derive tool handlers from a data type
-- Usage: $(deriveToolHandler ''MyTool 'handleTool)
deriveToolHandler :: Name -> Name -> Q Exp
deriveToolHandler typeName handlerName = 
  deriveToolHandlerWithDescription typeName handlerName []

mkToolDefWithDescription :: [(String, String)] -> Con -> Q Exp
mkToolDefWithDescription descriptions con = 
  case con of
    NormalC name [] -> do
      let toolName = T.pack . toSnakeCase . nameBase $ name
      let constructorName = nameBase name
      let description = case lookup constructorName descriptions of
            Just desc -> desc
            Nothing   -> constructorName
      [| ToolDefinition
          { toolDefinitionName = $(litE $ stringL $ T.unpack toolName)
          , toolDefinitionDescription = $(litE $ stringL description)
          , toolDefinitionInputSchema = InputSchemaDefinitionObject
              { properties = []
              , required = []
              }
          } |]
    RecC name fields -> do
      let toolName = T.pack . toSnakeCase . nameBase $ name
      let constructorName = nameBase name
      let description = case lookup constructorName descriptions of
            Just desc -> desc
            Nothing   -> constructorName
      props <- sequence $ map (mkProperty descriptions) fields
      requiredFields <- return $ map (\(fieldName, _, fieldType) ->
        let isOptional = case fieldType of
              AppT (ConT n) _ -> nameBase n == "Maybe"
              _               -> False
        in if isOptional then Nothing else Just (nameBase fieldName)
        ) fields
      let required = [f | Just f <- requiredFields]
      [| ToolDefinition
          { toolDefinitionName = $(litE $ stringL $ T.unpack toolName)
          , toolDefinitionDescription = $(litE $ stringL description)
          , toolDefinitionInputSchema = InputSchemaDefinitionObject
              { properties = $(return $ ListE props)
              , required = $(return $ ListE $ map (LitE . StringL) required)
              }
          } |]
    _ -> fail "Unsupported constructor type for tools"


mkProperty :: [(String, String)] -> (Name, Bang, Type) -> Q Exp
mkProperty descriptions (fieldName, _, fieldType) = do
  let fieldStr = nameBase fieldName
  let description = case lookup fieldStr descriptions of
        Just desc -> desc
        Nothing   -> fieldStr
  let jsonType = case fieldType of
        ConT n | nameBase n == "Int" -> "integer"
        ConT n | nameBase n == "Integer" -> "integer"
        ConT n | nameBase n == "Double" -> "number"
        ConT n | nameBase n == "Float" -> "number"
        ConT n | nameBase n == "Bool" -> "boolean"
        AppT (ConT maybeType) innerType | nameBase maybeType == "Maybe" ->
          case innerType of
            ConT n | nameBase n == "Int"     -> "integer"
            ConT n | nameBase n == "Integer" -> "integer"
            ConT n | nameBase n == "Double"  -> "number"
            ConT n | nameBase n == "Float"   -> "number"
            ConT n | nameBase n == "Bool"    -> "boolean"
            _                                -> "string"
        _ -> "string"
  [| ($(litE $ stringL fieldStr), InputSchemaDefinitionProperty
      { propertyType = $(litE $ stringL jsonType)
      , propertyDescription = $(litE $ stringL description)
      }) |]

mkToolCase :: Name -> Con -> Q Clause
mkToolCase handlerName (NormalC name []) = do
  let toolName = T.pack . toSnakeCase . nameBase $ name
  clause [litP $ stringL $ T.unpack toolName]
    (normalB [| do
        content <- $(varE handlerName) $(conE name)
        pure $ Right content |])
    []
mkToolCase handlerName (RecC name fields) = do
  let toolName = T.pack . toSnakeCase . nameBase $ name
  body <- mkRecordCase name handlerName fields
  clause [litP $ stringL $ T.unpack toolName] (normalB (return body)) []
mkToolCase _ _ = fail "Unsupported constructor type for tools"
