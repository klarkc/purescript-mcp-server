{-# LANGUAGE OverloadedStrings   #-}
{-# LANGUAGE ScopedTypeVariables #-}

module MCP.Server
  ( -- * Server Runtime
    runMcpServerStdIn
  , runMcpServerWithTransport

    -- * Transport Support
  , module MCP.Server.Transport.Types
  , module MCP.Server.Transport.Stdio
  , module MCP.Server.Transport.Http

    -- * Utility Functions
  , jsonValueToText

    -- * Re-exports
  , module MCP.Server.Types
  ) where

import           Control.Monad.IO.Class (MonadIO)
import           Data.Aeson
import           Data.Text              (Text)
import qualified Data.Text              as T

import           MCP.Server.Transport.Types
import           MCP.Server.Transport.Stdio
import           MCP.Server.Transport.Http
import           MCP.Server.Types

-- | Convert JSON Value to Text representation suitable for handlers
jsonValueToText :: Value -> Text
jsonValueToText (String t) = t
jsonValueToText (Number n) = 
    -- Check if it's a whole number, if so format as integer
    if fromInteger (round n) == n
        then T.pack $ show (round n :: Integer)
        else T.pack $ show n
jsonValueToText (Bool True) = "true"
jsonValueToText (Bool False) = "false"
jsonValueToText Null = ""
jsonValueToText v = T.pack $ show v

-- | Run an MCP server with a specific transport
runMcpServerWithTransport :: (MonadIO m, McpTransport t) 
                         => t                      -- ^ Transport configuration  
                         -> McpServerInfo          -- ^ Server information
                         -> McpServerHandlers m    -- ^ Message handlers
                         -> m TransportResult      -- ^ Result of transport operation
runMcpServerWithTransport transport serverInfo handlers = 
  runTransport transport serverInfo handlers


-- | Run an MCP server using stdin/stdout
runMcpServerStdIn :: McpServerInfo -> McpServerHandlers IO -> IO ()
runMcpServerStdIn serverInfo handlers = do
  _ <- runMcpServerWithTransport StdioTransport serverInfo handlers
  return ()

