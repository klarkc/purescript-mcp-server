{-# LANGUAGE OverloadedStrings #-}

module Types where

import           Data.Text (Text)

data SimpleTool
    = GetValue { key :: Text }
    | SetValue { key :: Text, value :: Text }
    deriving (Show, Eq)
