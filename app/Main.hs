module Main where

import           Data.Maybe                           (fromMaybe)
import           Data.String                          (fromString)
import           Network.Wai.Handler.Warp             (run)
import           Network.Wai.Middleware.RequestLogger (logStdout)
import           System.Environment                   (getEnv, lookupEnv)

import           API

main :: IO ()
main = do
    token  <- fromString <$> getEnv "CHANNEL_TOKEN"
    secret <- fromString <$> getEnv "CHANNEL_SECRET"
    port   <- fmap read  <$> lookupEnv "PORT"
    run (fromMaybe 8000 port) $ logStdout $ app token secret
