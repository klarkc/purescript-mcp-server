{-# LANGUAGE OverloadedStrings #-}

module Types where

import           Data.Text (Text)

-- High-level data type definitions from SPEC.md

data MyPrompt
    = Recipe { idea :: Text }
    | Shopping { description :: Text }
    deriving (Show, Eq)

data MyResource
    = ProductCategories
    | SaleItems
    | HeadlineBannerAd
    deriving (Show, Eq)

data MyTool
    = SearchForProduct { q :: Text, category :: Maybe Text }
    | AddToCart { sku :: Text }
    | Checkout
    deriving (Show, Eq)
