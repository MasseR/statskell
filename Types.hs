{-# Language TemplateHaskell #-}
module Types (
    Bucket
  , Value
  , Type (..)
  , Consolidation (..)
  , Stats (..)
  , State
  , bucket
  , value
  , consolidate
  , type_
  , module Data.Text.Lazy
  , module Control.Concurrent.STM
) where

import Data.Text.Lazy (Text)
import Data.Map (Map)
import Control.Concurrent.STM (TVar)
import Data.Lens.Template

type Bucket = Text
type Value = Double
data Type = Gauge | Absolute deriving Show
data Consolidation = Average | Max | Min | Sum deriving Show
data Stats = Stats {
    _bucket :: Bucket
  , _value :: Value
  , _type_ :: Type
  , _consolidate :: Consolidation
} deriving Show

type State = TVar (Map Bucket [Stats])

$(makeLens ''Stats)
