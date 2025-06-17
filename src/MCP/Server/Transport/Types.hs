{-# LANGUAGE RankNTypes #-}

module MCP.Server.Transport.Types
  ( -- * Transport Class
    McpTransport(..)
  , TransportConfig(..)
  
    -- * Transport Result
  , TransportResult(..)
  ) where

import           Control.Monad.IO.Class (MonadIO)
import           MCP.Server.Types

-- | Result of transport operations
data TransportResult = TransportResult
  { transportSuccess :: Bool
  , transportError   :: Maybe String
  } deriving (Show, Eq)

-- | Configuration for different transport types
data TransportConfig
  = StdioConfig
  deriving (Show, Eq)

-- | Transport abstraction class
class McpTransport t where
  -- | Run the MCP server with this transport
  runTransport :: (MonadIO m) 
               => t                      -- ^ Transport configuration
               -> McpServerInfo          -- ^ Server information
               -> McpServerHandlers m    -- ^ Message handlers
               -> m TransportResult      -- ^ Result of transport operation