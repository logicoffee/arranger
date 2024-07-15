{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE OverloadedStrings #-}

module Message where

import Arrange
import Data.Aeson
import Data.Text

newtype Events = Events [TextMessageEvent] deriving (Show)

data TextMessageEvent = TextMessageEvent
  { replyToken :: Text,
    message :: Text
  }
  deriving (Show)

data TextMessageReply = TextMessageReply
  { replyToken :: Text,
    messages :: [Text]
  }

eventToReply :: TextMessageEvent -> TextMessageReply
eventToReply (TextMessageEvent token message) = TextMessageReply token [reply]
  where
    reply = case textToAim message of
      Left r -> r
      Right (Aim aim comment) -> "【ねらい】\n" <> aim <> "\n\n【コメント】\n" <> comment

instance FromJSON Events where
  parseJSON = withObject "Events" $ \v -> do
    Events <$> v .: "events"

instance FromJSON TextMessageEvent where
  parseJSON = withObject "TextMessageEvent" $ \v -> do
    replyToken <- v .: "replyToken"
    message <- v .: "message"
    messageType :: Text <- message .: "type"
    case messageType of
      "text" -> do
        text <- message .: "text"
        return $ TextMessageEvent replyToken text
      _ -> fail "text type only acceptable"

instance ToJSON TextMessageReply where
  toJSON (TextMessageReply replyToken messages) =
    object
      [ "replyToken" .= replyToken,
        "messages" .= Prelude.map (\m -> object ["type" .= ("text" :: Text), "text" .= m]) messages
      ]
