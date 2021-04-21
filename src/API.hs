{-# LANGUAGE DataKinds                #-}
{-# LANGUAGE DisambiguateRecordFields #-}
{-# LANGUAGE NamedFieldPuns           #-}
{-# LANGUAGE OverloadedStrings        #-}
{-# LANGUAGE TypeOperators            #-}

module API where

import           Control.Monad          (forM_)
import           Control.Monad.IO.Class (liftIO)
import           Control.Monad.Reader   (ReaderT, ask, runReaderT)
import qualified Data.Text              as T
import           Line.Bot.Client
import           Line.Bot.Types         as B
import           Line.Bot.Webhook       as W
import           Servant

import           Arrange

type WebM = ReaderT ChannelToken Handler

type API = "arranger" :> Webhook

mkReply :: T.Text -> T.Text
mkReply m = case parseLeft m of
    Left l  -> l
    Right a -> aim $ findArrange a

handleEvent :: Event -> Line NoContent
handleEvent EventMessage { message = W.MessageText { text }, replyToken } = replyMessage replyToken [B.MessageText (mkReply text) Nothing]
handleEvent _ = return NoContent

handleEvents :: [Event] -> WebM NoContent
handleEvents events = do
    token <- ask
    _     <- liftIO $ forM_ events $ flip runLine token . handleEvent
    return NoContent

server :: ServerT API WebM
server = handleEvents . events

app :: ChannelToken -> ChannelSecret -> Application
app token secret = serveWithContext api context server_ where
    api = Proxy :: Proxy API
    pc = Proxy :: Proxy '[ChannelSecret]
    server_ = hoistServerWithContext api pc (`runReaderT` token) server
    context = secret :. EmptyContext
