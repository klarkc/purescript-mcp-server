# mcp-server

A fully-featured Haskell library for building [Model Context Protocol (MCP)](https://modelcontextprotocol.io/) servers.

## Features

- **Complete MCP Implementation**: Designed against the MCP 2024-11-05 server specification
- **Type-Safe API**: Leverage Haskell's type system for robust MCP servers
- **Multiple Abstractions**: Both low-level fine-grained control and high-level derived interfaces
- **Template Haskell Support**: Automatic handler derivation from data types
- **Stdio Transport Only**: HTTP/SSE coming soon!

## Supported MCP Features

- ✅ **Prompts**: User-controlled prompt templates with arguments
- ✅ **Resources**: Application-controlled readable resources  
- ✅ **Tools**: Model-controlled callable functions
- ✅ **Initialization Flow**: Complete protocol lifecycle with version negotiation
- ✅ **Error Handling**: Comprehensive error types and JSON-RPC error responses

## Quick Start

Add the library `mcp-server` to your cabal file:

```cabal
build-depends:
  mcp-server
```

Create a simple module, such as this example below:

```haskell
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TemplateHaskell #-}

import MCP.Server
import MCP.Server.Derive

-- Define your data types
data MyPrompt = Recipe { idea :: Text } | Shopping { items :: Text }
data MyResource = Menu | Specials  
data MyTool = Search { query :: Text } | Order { item :: Text }

-- Implement handlers
handlePrompt :: MyPrompt -> IO Content
handlePrompt (Recipe idea) = pure $ ContentText $ "Recipe for " <> idea
handlePrompt (Shopping items) = pure $ ContentText $ "Shopping list: " <> items

handleResource :: MyResource -> IO Content  
handleResource Menu = pure $ ContentText "Today's menu..."
handleResource Specials = pure $ ContentText "Daily specials..."

handleTool :: MyTool -> IO Content
handleTool (Search query) = pure $ ContentText $ "Search results for " <> query
handleTool (Order item) = pure $ ContentText $ "Ordered " <> item

-- Derive handlers automatically
main :: IO ()
main = runMcpServerStdIn serverInfo handlers
  where
    serverInfo = McpServerInfo
      { serverName = "My MCP Server"
      , serverVersion = "1.0.0" 
      , serverInstructions = "A sample MCP server"
      }
    handlers = McpServerHandlers
      { prompts = Just $(derivePromptHandler ''MyPrompt 'handlePrompt)
      , resources = Just $(deriveResourceHandler ''MyResource 'handleResource)  
      , tools = Just $(deriveToolHandler ''MyTool 'handleTool)
      }
```

## Custom Descriptions

You can provide custom descriptions for constructors and fields using the `*WithDescription` variants:

```haskell
-- Define descriptions for constructors and fields
descriptions :: [(String, String)]
descriptions = 
  [ ("Recipe", "Generate a recipe for a specific dish")     -- Constructor description
  , ("Search", "Search our menu database")                  -- Constructor description
  , ("idea", "The dish you want a recipe for")              -- Field description
  , ("query", "Search terms to find menu items")            -- Field description
  ]

-- Use in derivation
handlers = McpServerHandlers
  { prompts = Just $(derivePromptHandlerWithDescription ''MyPrompt 'handlePrompt descriptions)
  , tools = Just $(deriveToolHandlerWithDescription ''MyTool 'handleTool descriptions)
  , resources = Just $(deriveResourceHandlerWithDescription ''MyResource 'handleResource descriptions)
  }
```

## Manual Handler Implementation

For fine-grained control, implement handlers manually:

```haskell
import MCP.Server

-- Manual handler implementation
promptListHandler :: IO [PromptDefinition]
promptGetHandler :: PromptName -> [(ArgumentName, ArgumentValue)] -> IO (Either Error Content)
-- ... implement your custom logic

main :: IO ()
main = runMcpServerStdIn serverInfo handlers
  where
    handlers = McpServerHandlers
      { prompts = Just (promptListHandler, promptGetHandler)
      , resources = Nothing  -- Not supported
      , tools = Nothing      -- Not supported  
      }
```

## Examples

The library includes a few different examples:

- **`examples/Simple/`**: Basic key-value store using Template Haskell derivation
- **`examples/Complete/`**: Full-featured example with prompts, resources, and tools

## Docker Usage

I like to build and publish my MCP servers to Docker - which means that it's much easier to configure assistants such as Claude Desktop to run them. 

```bash
# Build the image
docker build -t haskell-mcp-server .

# Run different examples
docker run -i --entrypoint="/usr/local/bin/haskell-mcp-server" haskell-mcp-server
```

And then configure Claude by editing `claude_desktop_config.json`:

```json
{
    "mcpServers": {
       "haskell-mcp-server-example": {
            "command": "docker",
            "args": [
                "run",
                "-i",
                "--entrypoint=/usr/local/bin/haskell-mcp-server",
                "haskell-mcp-server"
            ]
        }
    }
}
```

## Documentation

- [MCP Specification](https://modelcontextprotocol.io/specification/2024-11-05/)
- [API Documentation](https://hackage.haskell.org/package/mcp-server)
- [Examples](examples/)

## Contributing

Contributions are welcome! Please see the issue tracker for open issues and feature requests.

## Disclaimer - AI Assistance

I am not sure whether there is any stigma associated with this but Claude helped me write a lot of this library. I started with a very specific specification of what I wanted to achieve and worked shoulder-to-shoulder with Claude to implement and refactor the library until I was happy with it. A few of the features such as the Derive functions are a little out of my comfort zone to have manually written, so I appreciated having an expert guide me here - however I do suspect that this implementation may be sub-par and I do intend to refactor and rewrite large pieces of this through regular maintenance.

## License

BSD-3-Clause