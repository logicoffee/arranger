module Types where

import Data.Text
import Servant.Client

data State = State
  { channelAccessToken :: ChannelAccessToken,
    clientEnv :: ClientEnv
  }

type ChannelAccessToken = Text
