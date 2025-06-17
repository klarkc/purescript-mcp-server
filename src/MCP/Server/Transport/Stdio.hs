{-# LANGUAGE OverloadedStrings   #-}
{-# LANGUAGE ScopedTypeVariables #-}

module MCP.Server.Transport.Stdio
  ( -- * STDIO Transport
    transportRunStdio
  ) where

import           Control.Monad          (when)
import           Control.Monad.IO.Class (MonadIO, liftIO)
import           Data.Aeson
import qualified Data.ByteString.Lazy   as BSL
import qualified Data.Text              as T
import qualified Data.Text.Encoding     as TE
import qualified Data.Text.IO           as TIO
import           System.IO              (hFlush, hPutStrLn, stderr, stdout)

import           MCP.Server.Handlers
import           MCP.Server.JsonRpc
import           MCP.Server.Types


-- | Transport-specific implementation for STDIO
transportRunStdio :: (MonadIO m) => McpServerInfo -> McpServerHandlers m -> m ()
transportRunStdio serverInfo handlers = do
  loop
  where
    loop = do
      input <- liftIO TIO.getLine
      when (not $ T.null $ T.strip input) $ do
        liftIO $ hPutStrLn stderr $ "Received request: " ++ T.unpack input
        case eitherDecode (BSL.fromStrict $ TE.encodeUtf8 input) of
          Left err -> liftIO $ hPutStrLn stderr $ "Parse error: " ++ err
          Right jsonValue -> do
            case parseJsonRpcMessage jsonValue of
              Left err -> liftIO $ hPutStrLn stderr $ "JSON-RPC parse error: " ++ err
              Right message -> do
                liftIO $ hPutStrLn stderr $ "Processing message: " ++ show (getMessageSummary message)
                response <- handleMcpMessage serverInfo handlers message
                case response of
                  Just responseMsg -> do
                    liftIO $ hPutStrLn stderr $ "Sending response for: " ++ show (getMessageSummary message)
                    let responseText = TE.decodeUtf8 $ BSL.toStrict $ encode $ encodeJsonRpcMessage responseMsg
                    liftIO $ TIO.putStrLn responseText
                    liftIO $ hFlush stdout
                  Nothing -> liftIO $ hPutStrLn stderr $ "No response needed for: " ++ show (getMessageSummary message)
        loop


