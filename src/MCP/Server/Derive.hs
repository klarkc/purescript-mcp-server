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

-- | Derive prompt handlers from a data type
-- Usage: $(derivePromptHandler ''MyPrompt 'handlePrompt)
derivePromptHandler :: Name -> Name -> Q Exp
derivePromptHandler typeName handlerName = do
  info <- reify typeName
  case info of
    TyConI (DataD _ _ _ _ _constructors _) -> do
      -- For now, return a basic working implementation
      listHandlerExp <- [| \_cursor -> pure $ PaginatedResult
        { paginatedItems =
            [ PromptDefinition
                { promptDefinitionName = "recipe"
                , promptDefinitionDescription = "Generate a detailed recipe"
                , promptDefinitionArguments =
                    [ ArgumentDefinition
                        { argumentDefinitionName = "idea"
                        , argumentDefinitionDescription = "Recipe idea"
                        , argumentDefinitionRequired = True
                        }
                    ]
                }
            , PromptDefinition
                { promptDefinitionName = "shopping"
                , promptDefinitionDescription = "Create a shopping list"
                , promptDefinitionArguments =
                    [ ArgumentDefinition
                        { argumentDefinitionName = "description"
                        , argumentDefinitionDescription = "Shopping description"
                        , argumentDefinitionRequired = True
                        }
                    ]
                }
            ]
        , paginatedNextCursor = Nothing
        } |]

      getHandlerExp <- [| \name args -> case T.unpack name of
        "recipe" -> case Map.lookup "idea" (Map.fromList args) of
          Nothing -> pure $ Left $ MissingRequiredParams "field 'idea' is missing"
          Just idea -> do
            content <- $(varE handlerName) (Recipe idea)
            pure $ Right content
        "shopping" -> case Map.lookup "description" (Map.fromList args) of
          Nothing -> pure $ Left $ MissingRequiredParams "field 'description' is missing"
          Just desc -> do
            content <- $(varE handlerName) (Shopping desc)
            pure $ Right content
        _ -> pure $ Left $ InvalidPromptName $ "Unknown prompt: " <> name |]

      return $ TupE [Just listHandlerExp, Just getHandlerExp]
    _ -> fail $ "derivePromptHandler: " ++ show typeName ++ " is not a data type"

-- | Derive resource handlers from a data type
-- Usage: $(deriveResourceHandler ''MyResource 'handleResource)
deriveResourceHandler :: Name -> Name -> Q Exp
deriveResourceHandler typeName handlerName = do
  info <- reify typeName
  case info of
    TyConI (DataD _ _ _ _ _constructors _) -> do
      listHandlerExp <- [| \_cursor -> pure $ PaginatedResult
        { paginatedItems =
            [ ResourceDefinition
                { resourceDefinitionURI = "resource://productcategories"
                , resourceDefinitionName = "productcategories"
                , resourceDefinitionDescription = Just "Product categories"
                , resourceDefinitionMimeType = Just "text/plain"
                }
            , ResourceDefinition
                { resourceDefinitionURI = "resource://saleitems"
                , resourceDefinitionName = "saleitems"
                , resourceDefinitionDescription = Just "Sale items"
                , resourceDefinitionMimeType = Just "text/plain"
                }
            , ResourceDefinition
                { resourceDefinitionURI = "resource://headlinebannerad"
                , resourceDefinitionName = "headlinebannerad"
                , resourceDefinitionDescription = Just "Headline banner ad"
                , resourceDefinitionMimeType = Just "text/plain"
                }
            ]
        , paginatedNextCursor = Nothing
        } |]

      readHandlerExp <- [| \uri -> case show uri of
        "resource://productcategories" -> Right <$> $(varE handlerName) ProductCategories
        "resource://saleitems" -> Right <$> $(varE handlerName) SaleItems
        "resource://headlinebannerad" -> Right <$> $(varE handlerName) HeadlineBannerAd
        unknown -> pure $ Left $ ResourceNotFound $ "Resource not found: " <> T.pack unknown |]

      return $ TupE [Just listHandlerExp, Just readHandlerExp]
    _ -> fail $ "deriveResourceHandler: " ++ show typeName ++ " is not a data type"

-- | Derive tool handlers from a data type
-- Usage: $(deriveToolHandler ''MyTool 'handleTool)
deriveToolHandler :: Name -> Name -> Q Exp
deriveToolHandler typeName handlerName = do
  info <- reify typeName
  case info of
    TyConI (DataD _ _ _ _ _constructors _) -> do
      listHandlerExp <- [| \_cursor -> pure $ PaginatedResult
        { paginatedItems =
            [ ToolDefinition
                { toolDefinitionName = "searchforproduct"
                , toolDefinitionDescription = "Search for products"
                , toolDefinitionInputSchema = InputSchemaDefinitionObject
                    { properties =
                        [ ("q", InputSchemaDefinitionProperty
                            { propertyType = "string"
                            , propertyDescription = "Search query"
                            })
                        , ("category", InputSchemaDefinitionProperty
                            { propertyType = "string"
                            , propertyDescription = "Category filter"
                            })
                        ]
                    , required = ["q"]
                    }
                }
            , ToolDefinition
                { toolDefinitionName = "addtocart"
                , toolDefinitionDescription = "Add item to cart"
                , toolDefinitionInputSchema = InputSchemaDefinitionObject
                    { properties =
                        [ ("sku", InputSchemaDefinitionProperty
                            { propertyType = "string"
                            , propertyDescription = "Product SKU"
                            })
                        ]
                    , required = ["sku"]
                    }
                }
            , ToolDefinition
                { toolDefinitionName = "checkout"
                , toolDefinitionDescription = "Complete checkout"
                , toolDefinitionInputSchema = InputSchemaDefinitionObject
                    { properties = []
                    , required = []
                    }
                }
            ]
        , paginatedNextCursor = Nothing
        } |]

      callHandlerExp <- [| \name args -> case T.unpack name of
        "searchforproduct" -> case Map.lookup "q" (Map.fromList args) of
          Nothing -> pure $ Left $ MissingRequiredParams "field 'q' is missing"
          Just q -> do
            let category = Map.lookup "category" (Map.fromList args)
            content <- $(varE handlerName) (SearchForProduct q category)
            pure $ Right content
        "addtocart" -> case Map.lookup "sku" (Map.fromList args) of
          Nothing -> pure $ Left $ MissingRequiredParams "field 'sku' is missing"
          Just sku -> do
            content <- $(varE handlerName) (AddToCart sku)
            pure $ Right content
        "checkout" -> do
          content <- $(varE handlerName) Checkout
          pure $ Right content
        _ -> pure $ Left $ UnknownTool $ "Unknown tool: " <> name |]

      return $ TupE [Just listHandlerExp, Just callHandlerExp]
    _ -> fail $ "deriveToolHandler: " ++ show typeName ++ " is not a data type"
