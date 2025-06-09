{-# LANGUAGE OverloadedStrings #-}

module TestTypes where

import           Data.Text  (Text)
import qualified Data.Text  as T
import           MCP.Server (Content(..))

-- Test data types for end-to-end testing
data TestPrompt
    = SimplePrompt { message :: Text }
    | ComplexPrompt { title :: Text, priority :: Int, urgent :: Bool }
    | OptionalPrompt { required :: Text, optional :: Maybe Int }
    deriving (Show, Eq)

data TestResource
    = ConfigFile
    | DatabaseConnection
    | UserProfile
    deriving (Show, Eq)

data TestTool
    = Echo { text :: Text }
    | Calculate { operation :: Text, x :: Int, y :: Int }
    | Toggle { flag :: Bool }
    | Search { query :: Text, limit :: Maybe Int, caseSensitive :: Maybe Bool }
    deriving (Show, Eq)

-- Handler functions
handleTestPrompt :: TestPrompt -> IO Content
handleTestPrompt (SimplePrompt msg) = 
    pure $ ContentText $ "Simple prompt: " <> msg
handleTestPrompt (ComplexPrompt title prio urgent) = 
    pure $ ContentText $ "Complex prompt: " <> title <> " (priority=" <> T.pack (show prio) <> ", urgent=" <> T.pack (show urgent) <> ")"
handleTestPrompt (OptionalPrompt req opt) = 
    pure $ ContentText $ "Optional prompt: " <> req <> maybe "" ((" optional=" <>) . T.pack . show) opt

handleTestResource :: TestResource -> IO Content
handleTestResource ConfigFile = 
    pure $ ContentText "Config file contents: debug=true, timeout=30"
handleTestResource DatabaseConnection = 
    pure $ ContentText "Database at localhost:5432"
handleTestResource UserProfile = 
    pure $ ContentText "User profile for ID 123"

handleTestTool :: TestTool -> IO Content
handleTestTool (Echo text) = 
    pure $ ContentText $ "Echo: " <> text
handleTestTool (Calculate op x y) = 
    let result = case op of
            "add" -> x + y
            "multiply" -> x * y
            "subtract" -> x - y
            _ -> 0
    in pure $ ContentText $ T.pack (show result)
handleTestTool (Toggle flag) = 
    pure $ ContentText $ "Flag is now: " <> T.pack (show (not flag))
handleTestTool (Search query limit caseSens) = 
    pure $ ContentText $ "Search results for '" <> query <> "'" <>
        maybe "" ((" (limit=" <>) . (<> ")") . T.pack . show) limit <>
        maybe "" ((" (case-sensitive=" <>) . (<> ")") . T.pack . show) caseSens

-- Test descriptions for custom description functionality
testDescriptions :: [(String, String)]
testDescriptions = 
    [ ("Echo", "Echoes the input text back to the user")
    , ("Calculate", "Performs mathematical calculations")
    , ("text", "The text to echo back")
    , ("operation", "The mathematical operation to perform")
    , ("x", "The first number")
    , ("y", "The second number")
    ]