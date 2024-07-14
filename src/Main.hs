module Main where

import API
import Client (getClientEnv)
import Control.Monad.Trans.Reader (runReaderT)
import Data.Text
import Network.Wai.Handler.Warp
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
  run 8000 $ app s

getAccessToken :: IO ChannelAccessToken
getAccessToken = pack <$> getEnv "CHANNEL_ACCESS_TOKEN"
