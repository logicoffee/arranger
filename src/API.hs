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
import Data.Text
import Data.Text.Encoding (decodeUtf8, encodeUtf8)
import Message
import Network.Wai.Handler.Warp
import Servant
import Servant.API
import Servant.Client
import Types

type AppM = ReaderT State Handler

type Server' a = ServerT a AppM

type HealthzAPI = Get '[PlainText] Text

type ArrangeAPI = Header' '[Required, Strict] "X-Line-Signature" Text :> ReqBody '[JSON] Text :> ReqBody '[JSON] Events :> PostNoContent

type API = HealthzAPI :<|> ArrangeAPI

healthzServer :: Server' HealthzAPI
healthzServer = return "healthz"

arrangeServer :: Server' ArrangeAPI
arrangeServer signature reqBody (Events events) = do
  accessToken <- asks channelAccessToken
  env <- asks clientEnv
  liftIO $
    if verifySignature accessToken reqBody signature
      then forM_ events (\event -> runClientM (replyClient accessToken (eventToReply event)) env)
      else print "failed to verify signature"
  return NoContent

verifySignature :: ChannelAccessToken -> Text -> Text -> Bool
verifySignature token reqBody signature = hmac (encodeUtf8 token) (encodeUtf8 reqBody) == encodeUtf8 signature
