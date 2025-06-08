{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TemplateHaskell   #-}

module MCP.Server.Derive
  ( -- * Template Haskell Derivation
    derivePromptHandler
  , deriveResourceHandler
  , deriveToolHandler
  ) where

import qualified Data.Map            as Map
import qualified Data.Text           as T
import           Language.Haskell.TH

import           MCP.Server.Types

-- Helper function to convert Clause to Match
clauseToMatch :: Clause -> Match
clauseToMatch (Clause ps b ds) = Match (case ps of [p] -> p; _ -> TupP ps) b ds

-- | Derive prompt handlers from a data type
-- Usage: $(derivePromptHandler ''MyPrompt 'handlePrompt)
derivePromptHandler :: Name -> Name -> Q Exp
derivePromptHandler typeName handlerName = do
  info <- reify typeName
  case info of
    TyConI (DataD _ _ _ _ constructors _) -> do
      -- Generate prompt definitions
      promptDefs <- sequence $ map mkPromptDef constructors
      
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
    _ -> fail $ "derivePromptHandler: " ++ show typeName ++ " is not a data type"

mkPromptDef :: Con -> Q Exp
mkPromptDef (NormalC name []) = do
  let promptName = T.toLower . T.pack . nameBase $ name
  [| PromptDefinition
      { promptDefinitionName = $(litE $ stringL $ T.unpack promptName)
      , promptDefinitionDescription = $(litE $ stringL $ "Handle " ++ nameBase name)
      , promptDefinitionArguments = []
      } |]
mkPromptDef (RecC name fields) = do
  let promptName = T.toLower . T.pack . nameBase $ name
  args <- sequence $ map mkArgDef fields
  [| PromptDefinition
      { promptDefinitionName = $(litE $ stringL $ T.unpack promptName)
      , promptDefinitionDescription = $(litE $ stringL $ "Handle " ++ nameBase name)
      , promptDefinitionArguments = $(return $ ListE args)
      } |]
mkPromptDef _ = fail "Unsupported constructor type"

mkArgDef :: (Name, Bang, Type) -> Q Exp
mkArgDef (fieldName, _, fieldType) = do
  let isOptional = case fieldType of
        AppT (ConT n) _ -> nameBase n == "Maybe"
        _ -> False
  [| ArgumentDefinition
      { argumentDefinitionName = $(litE $ stringL $ nameBase fieldName)
      , argumentDefinitionDescription = $(litE $ stringL $ nameBase fieldName)
      , argumentDefinitionRequired = $(if isOptional then [| False |] else [| True |])
      } |]

mkPromptCase :: Name -> Con -> Q Clause
mkPromptCase handlerName (NormalC name []) = do
  let promptName = T.toLower . T.pack . nameBase $ name
  clause [litP $ stringL $ T.unpack promptName]
    (normalB [| do
        content <- $(varE handlerName) $(conE name)
        pure $ Right content |])
    []
mkPromptCase handlerName (RecC name fields) = do
  let promptName = T.toLower . T.pack . nameBase $ name
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
  let isOptional = case fieldType of
        AppT (ConT n) _ -> nameBase n == "Maybe"
        _ -> False
  let fieldVar = mkName ("field" ++ show depth)
  
  continuation <- buildNestedFieldValidation conName handlerName remainingFields (depth + 1)
  
  if isOptional
    then [| do
        let $(varP fieldVar) = Map.lookup $(litE $ stringL fieldStr) (Map.fromList args)
        $(return continuation) |]
    else [| case Map.lookup $(litE $ stringL fieldStr) (Map.fromList args) of
        Just $(varP fieldVar) -> $(return continuation)
        Nothing -> pure $ Left $ MissingRequiredParams $ "field '" <> $(litE $ stringL fieldStr) <> "' is missing" |]

-- | Derive resource handlers from a data type
-- Usage: $(deriveResourceHandler ''MyResource 'handleResource)
deriveResourceHandler :: Name -> Name -> Q Exp
deriveResourceHandler typeName handlerName = do
  info <- reify typeName
  case info of
    TyConI (DataD _ _ _ _ constructors _) -> do
      -- Generate resource definitions
      resourceDefs <- sequence $ map mkResourceDef constructors
      
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
    _ -> fail $ "deriveResourceHandler: " ++ show typeName ++ " is not a data type"

mkResourceDef :: Con -> Q Exp
mkResourceDef (NormalC name []) = do
  let resourceName = T.toLower . T.pack . nameBase $ name
  let resourceURI = "resource://" <> T.unpack resourceName
  [| ResourceDefinition
      { resourceDefinitionURI = $(litE $ stringL resourceURI)
      , resourceDefinitionName = $(litE $ stringL $ T.unpack resourceName)
      , resourceDefinitionDescription = Just $(litE $ stringL $ nameBase name)
      , resourceDefinitionMimeType = Just "text/plain"
      } |]
mkResourceDef _ = fail "Unsupported constructor type for resources"

mkResourceCase :: Name -> Con -> Q Clause
mkResourceCase handlerName (NormalC name []) = do
  let resourceName = T.toLower . T.pack . nameBase $ name
  let resourceURI = "resource://" <> T.unpack resourceName
  clause [litP $ stringL resourceURI]
    (normalB [| Right <$> $(varE handlerName) $(conE name) |])
    []
mkResourceCase _ _ = fail "Unsupported constructor type for resources"

-- | Derive tool handlers from a data type
-- Usage: $(deriveToolHandler ''MyTool 'handleTool)
deriveToolHandler :: Name -> Name -> Q Exp
deriveToolHandler typeName handlerName = do
  info <- reify typeName
  case info of
    TyConI (DataD _ _ _ _ constructors _) -> do
      -- Generate tool definitions
      toolDefs <- sequence $ map mkToolDef constructors
      
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
    _ -> fail $ "deriveToolHandler: " ++ show typeName ++ " is not a data type"

mkToolDef :: Con -> Q Exp
mkToolDef (NormalC name []) = do
  let toolName = T.toLower . T.pack . nameBase $ name
  [| ToolDefinition
      { toolDefinitionName = $(litE $ stringL $ T.unpack toolName)
      , toolDefinitionDescription = $(litE $ stringL $ nameBase name)
      , toolDefinitionInputSchema = InputSchemaDefinitionObject
          { properties = []
          , required = []
          }
      } |]
mkToolDef (RecC name fields) = do
  let toolName = T.toLower . T.pack . nameBase $ name
  props <- sequence $ map mkProperty fields
  requiredFields <- return $ map (\(fieldName, _, fieldType) ->
    let isOptional = case fieldType of
          AppT (ConT n) _ -> nameBase n == "Maybe"
          _ -> False
    in if isOptional then Nothing else Just (nameBase fieldName)
    ) fields
  let required = [f | Just f <- requiredFields]
  [| ToolDefinition
      { toolDefinitionName = $(litE $ stringL $ T.unpack toolName)
      , toolDefinitionDescription = $(litE $ stringL $ nameBase name)
      , toolDefinitionInputSchema = InputSchemaDefinitionObject
          { properties = $(return $ ListE props)
          , required = $(return $ ListE $ map (LitE . StringL) required)
          }
      } |]
mkToolDef _ = fail "Unsupported constructor type for tools"

mkProperty :: (Name, Bang, Type) -> Q Exp
mkProperty (fieldName, _, _) = do
  let fieldStr = nameBase fieldName
  [| ($(litE $ stringL fieldStr), InputSchemaDefinitionProperty
      { propertyType = "string"
      , propertyDescription = $(litE $ stringL fieldStr)
      }) |]

mkToolCase :: Name -> Con -> Q Clause
mkToolCase handlerName (NormalC name []) = do
  let toolName = T.toLower . T.pack . nameBase $ name
  clause [litP $ stringL $ T.unpack toolName]
    (normalB [| do
        content <- $(varE handlerName) $(conE name)
        pure $ Right content |])
    []
mkToolCase handlerName (RecC name fields) = do
  let toolName = T.toLower . T.pack . nameBase $ name
  body <- mkRecordCase name handlerName fields
  clause [litP $ stringL $ T.unpack toolName] (normalB (return body)) []
mkToolCase _ _ = fail "Unsupported constructor type for tools"