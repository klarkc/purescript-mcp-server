{-# LANGUAGE OverloadedStrings #-}

module Main where

import qualified Data.Map    as Map
import           Data.Text   (Text)
import qualified Data.Text   as T
import           MCP.Server
import           Network.URI (URI)
import           System.IO   (hPutStrLn, stderr)

-- High-level data type definitions from SPEC.md

data MyPrompt
    = Recipe { idea :: Text }
    | Shopping { description :: Text }
    deriving (Show, Eq)

data MyResource
    = ProductCategories
    | SaleItems
    | HeadlineBannerAd
    deriving (Show, Eq)

data MyTool
    = SearchForProduct { q :: Text, category :: Maybe Text }
    | AddToCart { sku :: Text }
    | Checkout
    deriving (Show, Eq)

-- High-level handler functions

handlePrompt :: MyPrompt -> IO Content
handlePrompt (Recipe idea) =
    pure $ ContentText $ "Recipe prompt for " <> idea <> ": Start by gathering fresh ingredients..."
handlePrompt (Shopping description) =
    pure $ ContentText $ "Shopping prompt for " <> description <> ": Create a detailed shopping list..."

handleResource :: MyResource -> IO Content
handleResource ProductCategories = pure $ ContentText "Fresh Produce, Dairy, Bakery, Meat & Seafood, Frozen Foods"
handleResource SaleItems = pure $ ContentText "Organic Apples $2.99/lb, Free Range Eggs $4.50/dozen, Artisan Bread $3.25/loaf"
handleResource HeadlineBannerAd = pure $ ContentText "🛒 Weekly Special: 20% off all organic produce! 🥕🥬🍎"

handleTool :: MyTool -> IO Content
handleTool (SearchForProduct q category) =
    case category of
        Nothing -> pure $ ContentText $ "Search results for '" <> q <> "': Found 15 products across all categories"
        Just cat -> pure $ ContentText $ "Search results for '" <> q <> "' in " <> cat <> ": Found 8 products"
handleTool (AddToCart sku) = pure $ ContentText $ "Added item " <> sku <> " to your cart. Cart total: 3 items"
handleTool Checkout = pure $ ContentText "Checkout completed! Order #12345 confirmed. Thank you for shopping with us!"

-- Manual derivation (we'll replace this with Template Haskell later)

promptListHandler :: Maybe Cursor -> IO (PaginatedResult [PromptDefinition])
promptListHandler _ = pure $ PaginatedResult
    { paginatedItems =
        [ PromptDefinition
            { promptDefinitionName = "recipe"
            , promptDefinitionDescription = "Generate a detailed recipe for a particular idea"
            , promptDefinitionArguments =
                [ ArgumentDefinition
                    { argumentDefinitionName = "idea"
                    , argumentDefinitionDescription = "inspiring idea for the recipe"
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
                    , argumentDefinitionDescription = "description of what to shop for"
                    , argumentDefinitionRequired = True
                    }
                ]
            }
        ]
    , paginatedNextCursor = Nothing
    }

promptGetHandler :: PromptName -> [(ArgumentName, ArgumentValue)] -> IO (Either Error Content)
promptGetHandler "recipe" args = do
    case Map.lookup "idea" (Map.fromList args) of
        Nothing -> pure $ Left $ MissingRequiredParams "field 'idea' is missing"
        Just idea -> do
            content <- handlePrompt (Recipe idea)
            pure $ Right content
promptGetHandler "shopping" args = do
    case Map.lookup "description" (Map.fromList args) of
        Nothing -> pure $ Left $ MissingRequiredParams "field 'description' is missing"
        Just desc -> do
            content <- handlePrompt (Shopping desc)
            pure $ Right content
promptGetHandler name _ = pure $ Left $ InvalidPromptName $ "Unknown prompt: " <> name

resourceListHandler :: Maybe Cursor -> IO (PaginatedResult [ResourceDefinition])
resourceListHandler _ = pure $ PaginatedResult
    { paginatedItems =
        [ ResourceDefinition
            { resourceDefinitionURI = "resource://productcategories"
            , resourceDefinitionName = "productcategories"
            , resourceDefinitionDescription = Just "Fresh produce, dairy, and other categories"
            , resourceDefinitionMimeType = Just "text/plain"
            }
        , ResourceDefinition
            { resourceDefinitionURI = "resource://saleitems"
            , resourceDefinitionName = "saleitems"
            , resourceDefinitionDescription = Just "Current sale items and discounts"
            , resourceDefinitionMimeType = Just "text/plain"
            }
        , ResourceDefinition
            { resourceDefinitionURI = "resource://headlinebannerad"
            , resourceDefinitionName = "headlinebannerad"
            , resourceDefinitionDescription = Just "Weekly promotional banner"
            , resourceDefinitionMimeType = Just "text/plain"
            }
        ]
    , paginatedNextCursor = Nothing
    }

resourceReadHandler :: URI -> IO (Either Error Content)
resourceReadHandler uri = do
    case show uri of
        "resource://productcategories" -> Right <$> handleResource ProductCategories
        "resource://saleitems" -> Right <$> handleResource SaleItems
        "resource://headlinebannerad" -> Right <$> handleResource HeadlineBannerAd
        unknown -> pure $ Left $ ResourceNotFound $ "Resource not found: " <> T.pack unknown

toolListHandler :: Maybe Cursor -> IO (PaginatedResult [ToolDefinition])
toolListHandler _ = pure $ PaginatedResult
    { paginatedItems =
        [ ToolDefinition
            { toolDefinitionName = "searchforproduct"
            , toolDefinitionDescription = "Search for products in the catalog"
            , toolDefinitionInputSchema =
                InputSchemaDefinitionObject
                    { properties =
                        [ ("q", InputSchemaDefinitionProperty
                            { propertyType = "string"
                            , propertyDescription = "Search query"
                            })
                        , ("category", InputSchemaDefinitionProperty
                            { propertyType = "string"
                            , propertyDescription = "Optional category filter"
                            })
                        ]
                    , required = ["q"]
                    }
            }
        , ToolDefinition
            { toolDefinitionName = "addtocart"
            , toolDefinitionDescription = "Add an item to shopping cart"
            , toolDefinitionInputSchema =
                InputSchemaDefinitionObject
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
            , toolDefinitionDescription = "Complete the checkout process"
            , toolDefinitionInputSchema =
                InputSchemaDefinitionObject
                    { properties = []
                    , required = []
                    }
            }
        ]
    , paginatedNextCursor = Nothing
    }

toolCallHandler :: ToolName -> [(ArgumentName, ArgumentValue)] -> IO (Either Error Content)
toolCallHandler "searchforproduct" args = do
    case Map.lookup "q" (Map.fromList args) of
        Nothing -> pure $ Left $ MissingRequiredParams "field 'q' is missing"
        Just q -> do
            let category = Map.lookup "category" (Map.fromList args)
            content <- handleTool (SearchForProduct q category)
            pure $ Right content
toolCallHandler "addtocart" args = do
    case Map.lookup "sku" (Map.fromList args) of
        Nothing -> pure $ Left $ MissingRequiredParams "field 'sku' is missing"
        Just sku -> do
            content <- handleTool (AddToCart sku)
            pure $ Right content
toolCallHandler "checkout" _ = do
    content <- handleTool Checkout
    pure $ Right content
toolCallHandler name _ = pure $ Left $ UnknownTool $ "Unknown tool: " <> name

main :: IO ()
main = do
    hPutStrLn stderr "Starting High-Level Grocery Store MCP Server..."
    hPutStrLn stderr "Using manually derived handlers (Template Haskell coming soon!)"
    hPutStrLn stderr "Ready for JSON-RPC communication"

    runMcpServerStdIn
        McpServerInfo
            { serverName = "High-Level Grocery Store MCP Server"
            , serverVersion = "0.2.0"
            , serverInstructions = "Advanced grocery store server with type-safe high-level handlers"
            }
        McpServerHandlers
            { prompts = Just (promptListHandler, promptGetHandler)
            , resources = Just (resourceListHandler, resourceReadHandler)
            , tools = Just (toolListHandler, toolCallHandler)
            }
