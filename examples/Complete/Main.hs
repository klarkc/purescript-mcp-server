{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TemplateHaskell   #-}

module Main where

import           MCP.Server
import           MCP.Server.Derive
import           System.IO         (hPutStrLn, stderr)
import           Types

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
handleTool (ComplexTool field1 field2 field3 field4 field5) =
    pure $ ContentText $ "Complex tool called with: " <> field1 <> ", " <> field2 <>
                        maybe "" (", " <>) field3 <> ", " <> field4 <>
                        maybe "" (", " <>) field5

main :: IO ()
main = do
    hPutStrLn stderr "Starting Template Haskell MCP Server..."
    hPutStrLn stderr "Using automatic Template Haskell derivation!"
    hPutStrLn stderr "Ready for JSON-RPC communication"

    -- Derive the handlers using Template Haskell
    let prompts = $(derivePromptHandler ''MyPrompt 'handlePrompt)
        resources = $(deriveResourceHandler ''MyResource 'handleResource)
        tools = $(deriveToolHandler ''MyTool 'handleTool)
     in runMcpServerStdIn
        McpServerInfo
            { serverName = "Template Haskell MCP Server"
            , serverVersion = "0.3.0"
            , serverInstructions = "MCP server using Template Haskell for automatic handler derivation"
            }
        McpServerHandlers
            { prompts = Just prompts
            , resources = Just resources
            , tools = Just tools
            }
