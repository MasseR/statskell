{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ViewPatterns #-}
module Main where

import Data.Text.Lazy (Text)

type Bucket = Text
type Value = Double
data Type = Counter | Absolute
data Aggregate = Average | Max | Min
data Stats = Stats Bucket Value Type Aggregate
