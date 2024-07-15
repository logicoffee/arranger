{-# LANGUAGE OverloadedStrings #-}

module Types where

import Data.ByteString.Char8 as B
import Data.Text as T
import Data.Text.Encoding (encodeUtf8)
import System.Environment (getEnv)
import Servant.API

data State = State
  { channelAccessToken :: ChannelAccessToken,
    channelSecret :: ChannelSecret
  }

initState :: IO State
initState = State <$> getChannelAccessToken <*> getChannelSecret

newtype ChannelAccessToken = ChannelAccessToken Text

type ChannelSecret = ByteString

instance ToHttpApiData ChannelAccessToken where
  toHeader (ChannelAccessToken t) = encodeUtf8 $ "Bearer " <> t
  toQueryParam (ChannelAccessToken t) = t

getChannelAccessToken :: IO ChannelAccessToken
getChannelAccessToken = ChannelAccessToken . T.pack <$> getEnv "CHANNEL_ACCESS_TOKEN"

getChannelSecret :: IO ChannelSecret
getChannelSecret = B.pack <$> getEnv "CHANNEL_SECRET"
