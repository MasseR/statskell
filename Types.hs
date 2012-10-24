{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# Language TemplateHaskell #-}
{-# Language DeriveGeneric #-}
{-# Language DefaultSignatures #-}
module Types (
    Bucket
  , Value
  , Type (..)
  , Consolidation (..)
  , Stats (..)
  , Statskell
  , Settings (..)
  , RRA(..)
  , Message(..)
  , Buffer(..)
  , BufferElement(..)
  , Archive(..)
  , Meta(..)
  , Query
  , consolidationFunc
  , runStatskellT
  , bucket
  , value
  , type_
  , module Data.Text.Lazy
  , module Control.Concurrent.STM
) where

import qualified Data.Vector.Unboxed as V
import Data.Text.Lazy (Text)
import Control.Concurrent.STM (TVar, TChan)
import Data.Lens.Template
import Control.Monad.Reader
import Network.Socket (PortNumber)
import GHC.Generics (Generic)
import Data.Serialize
import qualified Data.Text.Lazy.Encoding as TE

data Message = AddItem Stats | AddArchive Int Integer Consolidation deriving Show
type Bucket = Text
type Value = Double
type Query = RRA -> [BufferElement]
data Type = Gauge | Absolute deriving Show
data Stats = Stats {
    _bucket :: Bucket
  , _value :: Value
  , _type_ :: Type
  , _consolidate :: Consolidation
} deriving Show

data Settings = Settings {
    stats :: TVar RRA
  , errorChan :: TChan Text
  , port :: PortNumber
  , databaseDir :: FilePath
}
type ConsolidationFunc = [Double] -> Double
data Meta = Meta [Buffer] [Archive] deriving (Generic, Show)
data Archive = Archive {
    archiveElements :: !Int
  , archiveInterval :: !Integer
  , archiveConsolidation :: !Consolidation
  , archiveBuffers :: [Buffer]
  } deriving (Generic, Show)
data Consolidation = Sum | Average | Max | Min | Latest deriving (Generic, Show, Read, Eq)
data Buffer = Buffer {
    bufferName :: !Text
  , bufferElements :: [BufferElement]
  } deriving (Generic, Show, Ord, Eq)
data BufferElement = BufferElement {
    bufferElementTime :: !Integer
  , bufferElementValue :: !Double
  }deriving (Generic, Show, Ord, Eq)
data RRA = RRA !Meta deriving (Generic, Show)

newtype Statskell m a = Statskell (ReaderT Settings m a) deriving (Monad, MonadIO, MonadReader Settings)

runStatskellT :: Monad m =>  Statskell m a -> Settings -> m a
runStatskellT (Statskell statskell) = runReaderT statskell

$(makeLens ''Stats)

instance Serialize Text where
  put = put . TE.encodeUtf8
  get = TE.decodeUtf8 `fmap` get
instance Serialize RRA where
instance Serialize BufferElement where
instance Serialize Buffer where
instance Serialize Consolidation where
instance Serialize Archive where
instance Serialize Meta where

consolidationFunc :: Consolidation -> ConsolidationFunc
consolidationFunc Sum = V.sum . V.fromList
consolidationFunc Average = \xs -> let
  v = V.fromList xs
  l = V.length v
  in V.sum v / fromIntegral l
consolidationFunc Max = V.maximum . V.fromList
consolidationFunc Min = V.minimum . V.fromList
consolidationFunc Latest = last
