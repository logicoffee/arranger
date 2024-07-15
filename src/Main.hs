{-# LANGUAGE DataKinds #-}

module Main where

import API
import Control.Monad.Trans.Reader (runReaderT)
import Network.Wai.Handler.Warp
import Network.Wai.Logger (withStdoutLogger)
import Servant
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
  s <- initState
  withStdoutLogger $ \logger -> do
    let settings = setPort 8000 $ setLogger logger defaultSettings
    runSettings settings (app s)
