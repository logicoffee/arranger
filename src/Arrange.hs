{-# LANGUAGE OverloadedStrings #-}

module Arrange where

import Data.Map as Map
import Data.Text
import Text.Megaparsec
import Text.Megaparsec.Char.Lexer

textToAim :: Text -> Either Text Aim
textToAim x =
  case parseMaybe intParser x of
    Nothing -> Left "数字を入力してください"
    Just number -> case Map.lookup number arrange of
      Nothing -> Left "非対応の数字です"
      Just aim -> Right aim

intParser :: Parsec Text Text Int
intParser = do
  int <- decimal
  eof
  return int

type Arrange = Map Int Aim

data Aim = Aim
  { aim :: Text,
    comment :: Text
  }

arrange :: Arrange
arrange =
  fromList
    [ (2, Aim "D1" "Mad House とも呼ばれます。"),
      (3, Aim "S1 -> D1" "略"),
      (4, Aim "D2" "略"),
      (5, Aim "S1 -> D2" "略"),
      (6, Aim "D3" "略"),
      (7, Aim "S3 -> D2" "略"),
      (8, Aim "D4" "略"),
      (9, Aim "S1 -> D4" "略"),
      (10, Aim "D5" "略"),
      (11, Aim "S3 -> D4" "略"),
      (12, Aim "D6" "略"),
      (13, Aim "S5 -> D4" "略"),
      (14, Aim "D7" "略"),
      (15, Aim "S7 -> D4" "略"),
      (16, Aim "D8" "略"),
      (17, Aim "S1 -> D8" "略"),
      (18, Aim "D9" "略"),
      (19, Aim "S3 -> D8" "略"),
      (20, Aim "D10" "略"),
      (21, Aim "S5 -> D8" "略"),
      (22, Aim "D11" "略"),
      (23, Aim "S7 -> D8" "略"),
      (24, Aim "D12" "略"),
      (25, Aim "S9 -> D12" "略"),
      (26, Aim "D13" "略"),
      (27, Aim "S11 -> D8" "略"),
      (28, Aim "D14" "略"),
      (29, Aim "S13 -> D8" "略"),
      (30, Aim "D15" "略"),
      (31, Aim "S15 -> D8" "略"),
      (32, Aim "D16" "略"),
      (33, Aim "S1 -> D16" "略"),
      (34, Aim "D17" "略"),
      (35, Aim "S3 -> D16" "略"),
      (36, Aim "D18" "略"),
      (37, Aim "S5 -> D16" "略"),
      (38, Aim "D16" "略"),
      (39, Aim "S7 -> D16" "略"),
      (40, Aim "D20" "略")
    ]
