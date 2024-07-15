{-# LANGUAGE DataKinds #-}
{-# LANGUAGE OverloadedStrings #-}

module Client where

import Data.Text
import Message
import Network.HTTP.Client (Manager, newManager)
import Network.HTTP.Client.TLS (tlsManagerSettings)
import Servant
import Servant.API
import Servant.Client
import Types

reply :: ChannelAccessToken -> TextMessageReply -> IO (Either ClientError NoContent)
reply accessToken message = do
  env <- getClientEnv
  runClientM (replyClient accessToken message) env

type ReplyAPI =
  "v2"
    :> "bot"
    :> "message"
    :> "reply"
    :> Header' '[Required, Strict] "Authorization" ChannelAccessToken
    :> ReqBody '[JSON] TextMessageReply
    :> PostNoContent

api :: Proxy ReplyAPI
api = Proxy

replyClient :: ChannelAccessToken -> TextMessageReply -> ClientM NoContent
replyClient = client api

getClientEnv :: IO ClientEnv
getClientEnv = do
  manager <- newManager tlsManagerSettings
  return $ mkClientEnv manager baseUrl
  where
    baseUrl = BaseUrl Https "api.line.me" 443 ""
