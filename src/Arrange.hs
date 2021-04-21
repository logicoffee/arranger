{-# LANGUAGE OverloadedStrings #-}

module Arrange
    ( defaultArrange
    , findArrange
    , parseLeft
    , module Arrange.Value
    , module Arrange.Type
    ) where

import           Data.Attoparsec.Text
import           Data.List
import           Data.Maybe           (fromMaybe)
import qualified Data.Text            as T

import           Arrange.Type
import           Arrange.Value

defaultArrange :: Arrange
defaultArrange = Arrange 0 "" ""

findArrange :: Int -> Arrange
findArrange n
    | n <= 0 = defaultArrange
    | n >= 501 = defaultArrange
    | otherwise = fromMaybe defaultArrange $ find (\a -> left a == n) arranges

parseLeft :: T.Text -> Either T.Text Int
parseLeft m = case parseOnly decimal m of
    Left _  -> Left "数字を入力してください"
    Right x -> Right x
