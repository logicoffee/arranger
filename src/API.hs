{-# LANGUAGE DataKinds #-}
{-# LANGUAGE ExistentialQuantification #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE OverloadedStrings #-}

module API where

import Client
import Control.Monad (forM_, when)
import Control.Monad.IO.Class (liftIO)
import Control.Monad.Trans.Reader (ReaderT, asks)
import Crypto.Hash.SHA256
import Data.Aeson
import Data.ByteString
import Data.Text
import Data.Text.Encoding (decodeUtf8, encodeUtf8)
import Message
import Network.Wai (lazyRequestBody)
import Network.Wai.Handler.Warp
import Servant
import Servant.API
import Servant.Client
import Servant.Server.Internal
import Types

type AppM = ReaderT State Handler

type Server' a = ServerT a AppM

type HealthzAPI = Get '[PlainText] Text

type ArrangeAPI = Header' '[Optional, Strict] "X-Line-Signature" Text :> ReqBody' '[Optional, Lenient] '[JSON] RawRequestBody :> PostNoContent

type API = HealthzAPI :<|> ArrangeAPI

healthzServer :: Server' HealthzAPI
healthzServer = return "healthz"

arrangeServer :: Server' ArrangeAPI
arrangeServer Nothing _ = do
  liftIO $ print "X-Line-Signature doesn't exist"
  return NoContent
arrangeServer _ (Left err) = do
  liftIO $ print "Request body doesn't exist or something went wrong"
  return NoContent
arrangeServer (Just signature) (Right (RawRequestBody reqBody)) = do
  accessToken <- asks channelAccessToken
  env <- asks clientEnv
  liftIO $ case decodeStrict reqBody of
    Nothing -> print "failed to parse request body"
    Just (Events events) ->
      if verifySignature accessToken reqBody signature
        then forM_ events (\event -> runClientM (replyClient accessToken (eventToReply event)) env)
        else print "failed to verify signature"
  return NoContent

verifySignature :: ChannelAccessToken -> ByteString -> Text -> Bool
verifySignature token reqBody signature = hmac (encodeUtf8 token) reqBody == encodeUtf8 signature
