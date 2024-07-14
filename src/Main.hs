module Main where

import API
import Client (getClientEnv)
import Control.Monad.Trans.Reader (runReaderT)
import Data.Text
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
app s = serve api $ hoistServer api (\x -> runReaderT x s) server

main :: IO ()
main = do
  s <- State <$> getAccessToken <*> getClientEnv
  withStdoutLogger $ \logger -> do
    let settings = setPort 8000 $ setLogger logger defaultSettings
    runSettings settings (app s)

getAccessToken :: IO ChannelAccessToken
getAccessToken = pack <$> getEnv "CHANNEL_ACCESS_TOKEN"
