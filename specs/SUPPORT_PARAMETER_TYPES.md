# Support for better parameter types

Currently, the mcp-server library supports defining Tools, Prompts and Resources like the following:

```haskell
data SimpleTool
    = GetValue { key :: Text }
    | SetValue { key :: Text, value :: Text }
    deriving (Show, Eq)
```

And then deriving the tool handlers like so:

```haskell
main :: IO ()
main = do
    let handleTool :: SimpleTool -> IO Content
        handleTool (GetValue k) = pure $ ContentText "..."
        handleTool (SetValue k v) = pure $ ContentText "..."

    -- Derive the tool handlers using Template Haskell with descriptions
    let tools = $(deriveToolHandler ''SimpleTool 'handleTool)
     in runMcpServerStdIn
        McpServerInfo
            { serverName = "Simple Key-Value MCP Server"
            , serverVersion = "1.0.0"
            , serverInstructions = "A simple key-value store with GetValue and SetValue tools"
            }
        McpServerHandlers
            { prompts = Nothing     -- No prompts in this example
            , resources = Nothing   -- No resources in this example
            , tools = Just tools    -- Only tools in this example
            }
```

The downside of this approach is the reliance on field accessors in the constructor records, e.g. "key" and "value" are technically partial functions and prone to issues.

Although these are OK for small examples (and are nice and easy to understand), the better and more acceptable approach in haskell is to rather introduce additional data types for each Constructor's parameters, e.g.:

```haskell
-- Positional arguments
data GetValueParams = GetValueParams Text
data SetValueParams = SetValueParams Text Text

data SimpleTool
    = GetValue GetValueParams
    | SetValue SetValueParams
    deriving (Show, Eq)
```

Or potentially mixing and matching record fields here too:

```haskell
-- Using fields, but prefixing with the Type name, because maybe the user wants to use Lenses
data GetValueParams = GetValueParams { _gvpKey :: Text }
data SetValueParams = SetValueParams { _svpKey :: Text, _svpValue :: Text }

makeLenses ''GetValueParams
makeLenses ''SetValueParams

data SimpleTool
    = GetValue GetValueParams
    | SetValue SetValueParams
    deriving (Show, Eq)
```

This would then be reflected in the API like:

```haskell
main :: IO ()
main = do
    let handleTool :: SimpleTool -> IO Content
        handleTool (GetValue (GetValueParams key)) = pure $ ContentText "..."
        handleTool (SetValue (SetValueParams k v)) = pure $ ContentText "..."

    -- Derive the tool handlers using Template Haskell with descriptions
    let tools = $(deriveToolHandler ''SimpleTool 'handleTool)
     in runMcpServerStdIn
        McpServerInfo
            { serverName = "Simple Key-Value MCP Server"
            , serverVersion = "1.0.0"
            , serverInstructions = "A simple key-value store with GetValue and SetValue tools"
            }
        McpServerHandlers
            { prompts = Nothing     -- No prompts in this example
            , resources = Nothing   -- No resources in this example
            , tools = Just tools    -- Only tools in this example
            }
```

# Suggested approach to support parameter types

update the `derivePromptHandler`, `deriveResourceHandler` and `deriveToolHandler` functions to allow for support for multiple constructor parameters:
- record fields (current approach)
- positional parameters, which could be either primitive types (String, Int, etc) or data types holding (and describing parameters). 


