{-# LANGUAGE OverloadedStrings #-}

module Types where

import Data.Aeson
import Data.ByteString
import Data.Text
import Data.Text.Encoding (encodeUtf8)
import Servant.API
import Servant.Client

data State = State
  { channelAccessToken :: ChannelAccessToken,
    channelSecret :: ChannelSecret,
    clientEnv :: ClientEnv
  }

type ChannelSecret = ByteString

newtype ChannelAccessToken = ChannelAccessToken Text

instance ToHttpApiData ChannelAccessToken where
  toHeader (ChannelAccessToken t) = encodeUtf8 $ "Bearer " <> t
  toQueryParam (ChannelAccessToken t) = t

newtype RawRequestBody = RawRequestBody ByteString

instance FromJSON RawRequestBody where
  parseJSON v = return $ RawRequestBody $ toStrict $ encode v
