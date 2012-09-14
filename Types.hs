{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# Language TemplateHaskell #-}
module Types (
    Bucket
  , Value
  , Type (..)
  , Consolidation (..)
  , Stats (..)
  , Statskell
  , Settings (..)
  , runStatskellT
  , bucket
  , value
  , consolidate
  , type_
  , module Data.Text.Lazy
  , module Control.Concurrent.STM
) where

import Data.Text.Lazy (Text)
import Data.Map (Map)
import Control.Concurrent.STM (TVar, TChan)
import Data.Lens.Template
import Control.Monad.Reader
import Control.Monad

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

data Settings = Settings {
    stats :: TVar (Map Bucket [Stats])
  , errorChan :: TChan Text
  , port :: Int
  , databaseDir :: FilePath
}

newtype Statskell m a = Statskell (ReaderT Settings m a) deriving (Monad, MonadIO, MonadReader Settings)

runStatskellT :: Monad m =>  Statskell m a -> Settings -> m a
runStatskellT (Statskell statskell) = runReaderT statskell

$(makeLens ''Stats)
