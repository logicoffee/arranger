module Types where

import Data.Aeson
import Data.ByteString
import Data.Text
import Servant.Client

data State = State
  { channelAccessToken :: ChannelAccessToken,
    clientEnv :: ClientEnv
  }

type ChannelAccessToken = Text

newtype RawRequestBody = RawRequestBody ByteString

instance FromJSON RawRequestBody where
  parseJSON v = return $ RawRequestBody $ toStrict $ encode v
