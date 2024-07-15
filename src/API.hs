{-# LANGUAGE DataKinds #-}
{-# LANGUAGE ExistentialQuantification #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE PackageImports #-}
{-# LANGUAGE TypeFamilies #-}

module API where

import Client
import Control.Monad (forM_)
import Control.Monad.IO.Class (liftIO)
import Control.Monad.Trans.Reader (ReaderT, asks)
import Crypto.Hash.SHA256 (hmaclazy)
import Data.ByteString qualified as B
import Data.ByteString.Lazy qualified as BL
import Data.Kind (Type)
import Data.Maybe (fromMaybe)
import Data.String.Conversions (cs)
import Data.Text
import Message
import Network.HTTP.Types (HeaderName, hContentType)
import Network.Wai (lazyRequestBody, requestHeaders)
import Servant
import Servant.API.ContentTypes
import Servant.Server.Internal
import Types
import "base64-bytestring" Data.ByteString.Base64 (decodeLenient)

type AppM = ReaderT State Handler

type Server' a = ServerT a AppM

type HealthzAPI = Get '[PlainText] Text

type ArrangeAPI = ReqBodyWithSignature '[JSON] Events :> PostNoContent

type API = HealthzAPI :<|> ArrangeAPI

healthzServer :: Server' HealthzAPI
healthzServer = return "healthz"

arrangeServer :: Server' ArrangeAPI
arrangeServer (Events events) = do
  accessToken <- asks channelAccessToken
  liftIO $ forM_ events (reply accessToken . eventToReply)
  return NoContent

data ReqBodyWithSignature (contentTypes :: [Type]) (a :: Type)

instance (AllCTUnrender list a, HasServer api context, HasContextEntry context ChannelSecret) => HasServer (ReqBodyWithSignature list a :> api) context where
  type ServerT (ReqBodyWithSignature list a :> api) m = a -> ServerT api m
  hoistServerWithContext _ pc nt s = hoistServerWithContext (Proxy :: Proxy api) pc nt . s
  route Proxy context subserver =
    route (Proxy :: Proxy api) context $
      addBodyCheck subserver ctCheck bodyCheck
    where
      ctCheck = withRequest $ \request -> do
        let contentTypeH =
              fromMaybe "application/octet-stream" $
                lookup hContentType $
                  requestHeaders request
        case canHandleCTypeH (Proxy :: Proxy list) (cs contentTypeH) :: Maybe (BL.ByteString -> Either String a) of
          Nothing -> delayedFail err415
          Just f -> return f

      bodyCheck f = withRequest $ \request -> do
        rawBody <- liftIO $ lazyRequestBody request
        let signatureH = lookup hSignature $ requestHeaders request

        if validateReqBody signatureH rawBody
          then case f rawBody of
            Left e -> delayedFailFatal err400 {errBody = cs e}
            Right v -> return v
          else delayedFailFatal err401

      channelSecret :: ChannelSecret
      channelSecret = getContextEntry context

      hSignature :: HeaderName
      hSignature = "X-Line-Signature"

      validateReqBody :: Maybe B.ByteString -> BL.ByteString -> Bool
      validateReqBody digest body = digest' == Just (hmaclazy channelSecret body)
        where
          digest' = decodeLenient <$> digest
