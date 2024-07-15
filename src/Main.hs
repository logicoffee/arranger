{-# LANGUAGE DataKinds #-}

module Main where

import API
import Client (getClientEnv)
import Control.Monad.Trans.Reader (runReaderT)
import Data.ByteString.Char8 as B
import Data.Text as T
import Network.Wai.Handler.Warp
import Network.Wai.Logger (withStdoutLogger)
import Servant
import System.Environment
import Types

api :: Proxy API
api = Proxy

server :: Server' API
server = healthzServer :<|> arrangeServer

app :: State -> Application
app s = serveWithContext api context $ hoistServerWithContext api (Proxy :: Proxy '[ChannelSecret]) (\x -> runReaderT x s) server
  where
    context = channelSecret s :. EmptyContext

main :: IO ()
main = do
  s <- State <$> getChannelAccessToken <*> getChannelSecret <*> getClientEnv
  withStdoutLogger $ \logger -> do
    let settings = setPort 8000 $ setLogger logger defaultSettings
    runSettings settings (app s)

getChannelAccessToken :: IO ChannelAccessToken
getChannelAccessToken = ChannelAccessToken . T.pack <$> getEnv "CHANNEL_ACCESS_TOKEN"

getChannelSecret :: IO ChannelSecret
getChannelSecret = B.pack <$> getEnv "CHANNEL_SECRET"
