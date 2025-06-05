# haskell-mcp-server

A fully-featured Haskell library for building [Model Context Protocol (MCP)](https://modelcontextprotocol.io/) servers.

## Features

- **Complete MCP Implementation**: Full support for MCP 2024-11-05 specification
- **Type-Safe API**: Leverage Haskell's type system for robust MCP servers
- **Multiple Abstractions**: Both low-level fine-grained control and high-level derived interfaces
- **Template Haskell Support**: Automatic handler derivation from data types
- **JSON-RPC Transport**: Complete JSON-RPC 2.0 implementation with stdin/stdout communication
- **Pagination Support**: Cursor-based pagination for large result sets
- **Docker Ready**: Multi-stage Docker builds with individual executable deployment

## Supported MCP Features

- ✅ **Prompts**: User-controlled prompt templates with arguments
- ✅ **Resources**: Application-controlled readable resources  
- ✅ **Tools**: Model-controlled callable functions
- ✅ **Initialization Flow**: Complete protocol lifecycle with version negotiation
- ✅ **Error Handling**: Comprehensive error types and JSON-RPC error responses
- ✅ **Capabilities**: Proper capability negotiation with sub-capabilities

## Quick Start

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

## Manual Handler Implementation

For fine-grained control, implement handlers manually:

```haskell
import MCP.Server

-- Manual handler implementation
promptListHandler :: Maybe Cursor -> IO (PaginatedResult [PromptDefinition])
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

The library includes three complete examples:

- **`app/Main.hs`**: Basic low-level MCP server
- **`examples/HighLevel.hs`**: Manual high-level implementation
- **`examples/TemplateHaskell.hs`**: Automatic Template Haskell derivation

## Docker Usage

```bash
# Build the image
docker build -t haskell-mcp-server .

# Run different examples
docker run -i haskell-mcp-server  # Default server
docker run -i --entrypoint="/usr/local/bin/template-haskell-example" haskell-mcp-server
docker run -i --entrypoint="/usr/local/bin/high-level-example" haskell-mcp-server
```

## Protocol Compliance

This library implements the complete MCP specification:

- **Initialization**: Proper protocol version negotiation and capability exchange
- **JSON-RPC 2.0**: Full request/response/notification support with error handling
- **Capability Objects**: Correct capability format with sub-capabilities
- **Pagination**: Cursor-based pagination for scalable list operations
- **Type Safety**: All MCP types are properly modeled in Haskell's type system

## Documentation

- [MCP Specification](https://modelcontextprotocol.io/specification/2024-11-05/)
- [API Documentation](https://hackage.haskell.org/package/haskell-mcp-server)
- [Examples](examples/)

## Contributing

Contributions are welcome! Please see the issue tracker for open issues and feature requests.

## License

BSD-3-Clause