{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ViewPatterns #-}
module Parser where

import Types
import Control.Monad.Error
import Control.Monad.Identity (runIdentity)
import qualified Data.Text.Lazy as T
import Control.Monad.Maybe
import Control.Monad (msum)
import Safe

parseCmd :: Text -> Maybe Message
parseCmd msg = msum [
    fmap (\(e,i,c) -> AddArchive e i c) $ parseArchive msg
  , fmap AddItem $ parseStats msg
  ]

parseArchive :: Text -> Maybe (Int, Integer, Consolidation)
parseArchive msg = runIdentity $ runMaybeT $ do
  [archive, elements, interval, consolidation] <- parseMsg msg
  archive' <- parseHeader archive
  elements' <- MaybeT $ return $ readMay (T.unpack elements)
  interval' <- MaybeT $ return $ readMay (T.unpack interval)
  consolidation' <- MaybeT $ return $ readMay (T.unpack consolidation)
  return (elements', interval', consolidation')
  where
    parseMsg msg = case T.splitOn "|" msg of
                        ret@[_,_,_,_] -> return ret
                        _ -> fail ""
    parseHeader (T.stripPrefix "archive" -> Just _) = return "archive"
    parseHeader _ = fail ""

parseStats :: Text -> Maybe Stats
parseStats msg = runIdentity $ runMaybeT $ do
  [bucket, value, _type, aggregate] <- parseMsg msg
  _type' <- parseType _type
  value' <- parseValue value
  aggregate' <- parseAggregate aggregate
  return $ Stats bucket value' _type' aggregate'
  where
    parseMsg msg = case T.splitOn "|" msg of
                    ret@[_, _, _, _] -> return ret
                    _ -> fail ""
    parseType (T.stripPrefix "gauge" -> Just _) = return Gauge
    parseType (T.stripPrefix "absolute" -> Just _) = return Absolute
    parseType _type = fail ""
    parseValue value = case reads (T.unpack value) of
                            [(value', _)] -> return value'
                            _ -> fail ""
    parseAggregate (T.stripPrefix "average" -> Just _) = return Average
    parseAggregate (T.stripPrefix "max" -> Just _) = return Max
    parseAggregate (T.stripPrefix "min" -> Just _) = return Min
    parseAggregate (T.stripPrefix "sum" -> Just _) = return Sum
    parseAggregate aggregate = fail ""
