{-# LANGUAGE DeriveGeneric     #-}
{-# LANGUAGE DeriveLift        #-}
{-# LANGUAGE OverloadedStrings #-}

module Arrange.Type where

import           Data.Aeson
import           Data.Attoparsec.Text
import           Data.Text                  as T
import           GHC.Generics               (Generic)
import           Language.Haskell.TH.Syntax (Lift)

data Arrange = Arrange
    { left        :: Int
    , aim         :: Text
    , description :: Text
    } deriving (Generic, Lift)
type Arranges = [Arrange]
instance FromJSON Arrange
