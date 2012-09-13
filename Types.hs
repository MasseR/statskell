module Types (
    Bucket
  , Value
  , Type (..)
  , Aggregate (..)
  , Stats (..)
  , module Data.Text.Lazy
) where

import Data.Text.Lazy (Text)

type Bucket = Text
type Value = Double
data Type = Counter | Absolute deriving Show
data Aggregate = Average | Max | Min | Sum deriving Show
data Stats = Stats Bucket Value Type Aggregate deriving Show

