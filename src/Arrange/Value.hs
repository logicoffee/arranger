{-# LANGUAGE TemplateHaskell #-}

module Arrange.Value where

import           Data.Yaml.TH (decodeFile)

import           Arrange.Type

arranges :: Arranges
arranges = $$(decodeFile "arranges.yaml")
