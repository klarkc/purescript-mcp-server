{-# LANGUAGE OverloadedStrings   #-}
{-# LANGUAGE ScopedTypeVariables #-}

module MCP.Server.Transport.Http
  ( -- * HTTP Transport
    HttpTransport(..)
  , HttpConfig(..)
  , runMcpServerHttp
  , runMcpServerHttpWithConfig
  ) where

import           Control.Monad            (when)
import           Data.Aeson
import qualified Data.ByteString.Lazy     as BSL
import           Data.String              (IsString (fromString))
import           Data.Text                (Text)
import qualified Data.Text                as T
import qualified Data.Text.Encoding       as TE
import qualified Data.Vector              as V
import           Network.HTTP.Types
import qualified Network.Wai              as Wai
import qualified Network.Wai.Handler.Warp as Warp
import           System.IO                (hPutStrLn, stderr)

import           MCP.Server.Handlers
import           MCP.Server.JsonRpc
import           MCP.Server.Types

-- | HTTP transport configuration following MCP 2025-03-26 Streamable HTTP specification
data HttpConfig = HttpConfig
  { httpPort     :: Int      -- ^ Port to listen on
  , httpHost     :: String   -- ^ Host to bind to (default "localhost")
  , httpEndpoint :: String   -- ^ MCP endpoint path (default "/mcp")
  , httpVerbose  :: Bool     -- ^ Enable verbose logging (default False)
  } deriving (Show, Eq)

-- | Default HTTP configuration
defaultHttpConfig :: HttpConfig
defaultHttpConfig = HttpConfig
  { httpPort = 3000
  , httpHost = "localhost"
  , httpEndpoint = "/mcp"
  , httpVerbose = False
  }

-- | HTTP transport following MCP 2025-03-26 Streamable HTTP specification
data HttpTransport = HttpTransport HttpConfig
  deriving (Show, Eq)

-- Note: HTTP transport is constrained to IO only due to Warp requirements
-- For the generic interface, we'll use a more specific function

-- | Helper for conditional logging
logVerbose :: HttpConfig -> String -> IO ()
logVerbose config msg = when (httpVerbose config) $ hPutStrLn stderr msg

-- | Run an MCP server using HTTP transport with default config
runMcpServerHttp :: McpServerInfo -> McpServerHandlers IO -> IO ()
runMcpServerHttp = runMcpServerHttpWithConfig defaultHttpConfig

-- | Run an MCP server using HTTP transport with custom config
runMcpServerHttpWithConfig :: HttpConfig -> McpServerInfo -> McpServerHandlers IO -> IO ()
runMcpServerHttpWithConfig config serverInfo handlers = do
  let settings = Warp.setHost (fromString $ httpHost config) $
                 Warp.setPort (httpPort config) $
                 Warp.defaultSettings

  putStrLn $ "Starting MCP HTTP server on " ++ httpHost config ++ ":" ++ show (httpPort config) ++ httpEndpoint config
  Warp.runSettings settings (mcpApplication config serverInfo handlers)

-- | WAI Application for MCP over HTTP
mcpApplication :: HttpConfig -> McpServerInfo -> McpServerHandlers IO -> Wai.Application
mcpApplication config serverInfo handlers req respond = do
  -- Log the request
  logVerbose config $ "HTTP " ++ show (Wai.requestMethod req) ++ " " ++ T.unpack (TE.decodeUtf8 $ Wai.rawPathInfo req)

  -- Check if this is our MCP endpoint
  if TE.decodeUtf8 (Wai.rawPathInfo req) == T.pack (httpEndpoint config)
    then handleMcpRequest config serverInfo handlers req respond
    else respond $ Wai.responseLBS status404 [("Content-Type", "text/plain")] "Not Found"

-- | Handle MCP requests according to Streamable HTTP specification
handleMcpRequest :: HttpConfig -> McpServerInfo -> McpServerHandlers IO -> Wai.Request -> (Wai.Response -> IO Wai.ResponseReceived) -> IO Wai.ResponseReceived
handleMcpRequest config serverInfo handlers req respond = do
  case Wai.requestMethod req of
    -- GET requests for endpoint discovery
    "GET" -> do
      let discoveryResponse = object
            [ "name" .= serverName serverInfo
            , "version" .= serverVersion serverInfo
            , "description" .= serverInstructions serverInfo
            , "protocolVersion" .= ("2025-03-26" :: Text)
            , "capabilities" .= object
                [ "tools" .= object []
                , "prompts" .= object []
                , "resources" .= object []
                ]
            ]
      logVerbose config $ "Sending server discovery response: " ++ show discoveryResponse
      respond $ Wai.responseLBS
        status200
        [("Content-Type", "application/json"), ("Access-Control-Allow-Origin", "*")]
        (encode discoveryResponse)

    -- POST requests for JSON-RPC messages
    "POST" -> do
      -- Read request body
      body <- Wai.strictRequestBody req
      logVerbose config $ "Received POST body (" ++ show (BSL.length body) ++ " bytes): " ++ take 200 (show body)
      handleJsonRpcRequest config serverInfo handlers body respond

    -- OPTIONS for CORS preflight
    "OPTIONS" -> respond $ Wai.responseLBS
      status200
      [ ("Access-Control-Allow-Origin", "*")
      , ("Access-Control-Allow-Methods", "GET, POST, OPTIONS")
      , ("Access-Control-Allow-Headers", "Content-Type")
      ]
      ""

    -- Unsupported methods
    _ -> respond $ Wai.responseLBS
      status405
      [("Content-Type", "text/plain"), ("Allow", "GET, POST, OPTIONS")]
      "Method Not Allowed"

-- | Handle JSON-RPC request from HTTP body (supports batching)
handleJsonRpcRequest :: HttpConfig -> McpServerInfo -> McpServerHandlers IO -> BSL.ByteString -> (Wai.Response -> IO Wai.ResponseReceived) -> IO Wai.ResponseReceived
handleJsonRpcRequest config serverInfo handlers body respond = do
  case eitherDecode body of
    Left err -> do
      hPutStrLn stderr $ "JSON parse error: " ++ err
      respond $ Wai.responseLBS
        status400
        [("Content-Type", "application/json")]
        (encode $ object ["error" .= ("Invalid JSON" :: Text)])

    Right jsonValue -> do
      -- Try to parse as batch first (array), then as single message
      case jsonValue of
        Array batch -> handleJsonRpcBatch config serverInfo handlers (V.toList batch) respond
        singleValue -> handleSingleJsonRpc config serverInfo handlers singleValue respond

-- | Handle a single JSON-RPC message
handleSingleJsonRpc :: HttpConfig -> McpServerInfo -> McpServerHandlers IO -> Value -> (Wai.Response -> IO Wai.ResponseReceived) -> IO Wai.ResponseReceived
handleSingleJsonRpc config serverInfo handlers jsonValue respond = do
  case parseJsonRpcMessage jsonValue of
    Left err -> do
      hPutStrLn stderr $ "JSON-RPC parse error: " ++ err
      respond $ Wai.responseLBS
        status400
        [("Content-Type", "application/json")]
        (encode $ object ["error" .= ("Invalid JSON-RPC" :: Text)])

    Right message -> do
      logVerbose config $ "Processing HTTP message: " ++ show (getMessageSummary message)
      maybeResponse <- handleMcpMessage serverInfo handlers message

      case maybeResponse of
        Just responseMsg -> do
          let responseJson = encode $ encodeJsonRpcMessage responseMsg
          logVerbose config $ "Sending HTTP response for: " ++ show (getMessageSummary message)
          respond $ Wai.responseLBS
            status200
            [("Content-Type", "application/json"), ("Access-Control-Allow-Origin", "*")]
            responseJson

        Nothing -> do
          logVerbose config $ "No response needed for: " ++ show (getMessageSummary message)
          -- For notifications, return 200 with empty JSON object (per MCP spec)
          respond $ Wai.responseLBS 
            status200 
            [("Content-Type", "application/json"), ("Access-Control-Allow-Origin", "*")] 
            "{}"

-- | Handle JSON-RPC batch request (MCP 2025-03-26 feature)
handleJsonRpcBatch :: HttpConfig -> McpServerInfo -> McpServerHandlers IO -> [Value] -> (Wai.Response -> IO Wai.ResponseReceived) -> IO Wai.ResponseReceived
handleJsonRpcBatch config serverInfo handlers batch respond = do
  logVerbose config $ "Processing JSON-RPC batch with " ++ show (length batch) ++ " messages"

  -- Process each message in the batch
  responses <- mapM (processBatchMessage config serverInfo handlers) batch

  -- Filter out Nothing responses (notifications)
  let validResponses = [r | Just r <- responses]

  case validResponses of
    [] -> do
      logVerbose config "Batch contained only notifications, returning empty JSON array"
      respond $ Wai.responseLBS 
        status200 
        [("Content-Type", "application/json"), ("Access-Control-Allow-Origin", "*")] 
        "[]"
    _ -> do
      let responseJson = encode $ map encodeJsonRpcMessage validResponses
      logVerbose config $ "Sending batch response with " ++ show (length validResponses) ++ " responses"
      respond $ Wai.responseLBS
        status200
        [("Content-Type", "application/json"), ("Access-Control-Allow-Origin", "*")]
        responseJson

-- | Process a single message from a batch
processBatchMessage :: HttpConfig -> McpServerInfo -> McpServerHandlers IO -> Value -> IO (Maybe JsonRpcMessage)
processBatchMessage config serverInfo handlers jsonValue = do
  case parseJsonRpcMessage jsonValue of
    Left err -> do
      logVerbose config $ "Batch message parse error: " ++ err
      return Nothing -- Skip invalid messages in batch
    Right message -> do
      logVerbose config $ "Processing batch message: " ++ show (getMessageSummary message)
      handleMcpMessage serverInfo handlers message

