{-# LANGUAGE DeriveGeneric     #-}
{-# LANGUAGE DeriveLift        #-}
{-# LANGUAGE OverloadedStrings #-}

module Arrange.Type where

import           Data.Aeson
import           Data.Aeson.Types
import           Data.Attoparsec.Text
import           Data.Text                  as T
import           GHC.Generics               (Generic)
import           Language.Haskell.TH.Syntax (Lift)

data Arrange = Arrange
    { left        :: LeftP
    , aim         :: Text
    , description :: Text
    } deriving (Generic, Lift)
type Arranges = [Arrange]
data LeftP
    = Single Int
    | Range
        { from :: Int
        , to   :: Int
        } deriving (Generic, Lift)
instance FromJSON Arrange
instance FromJSON LeftP where
    parseJSON n@(Number _) = Single <$> parseJSON n
    parseJSON (Object v)   = Range <$> v .: "from" <*> v .: "to"
    parseJSON _            = parseFail ""
