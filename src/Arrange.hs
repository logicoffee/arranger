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
defaultArrange = Arrange (Single 0) "未対応の数字です" ""

findArrange :: Int -> Either T.Text Arrange
findArrange n
    | n <= 0 = Left "未対応の数字です"
    | n >= 501 = Left "未対応の数字です"
    | otherwise = Right $ fromMaybe defaultArrange $ find (matchArrange n) arranges

matchArrange :: Int -> Arrange -> Bool
matchArrange n a = case left a of
    Single i  -> i == n
    Range l u -> l <= n && n <= u


parseLeft :: T.Text -> Either T.Text Int
parseLeft m = case parseOnly decimal m of
    Left _  -> Left "数字を入力してね"
    Right x -> Right x
