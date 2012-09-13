{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ViewPatterns #-}
module Parser where

import Types
import Control.Monad.Error
import Control.Monad.Identity (runIdentity)
import qualified Data.Text.Lazy as T

parseMsg :: Text -> Either String Stats
parseMsg msg = runIdentity $ runErrorT $ do
  [bucket, value, _type, aggregate] <- parseMsg msg
  _type' <- parseType _type
  value' <- parseValue value
  aggregate' <- parseAggregate aggregate
  return $ Stats bucket value' _type' aggregate'
  where
    parseMsg msg = case T.splitOn "|" msg of
                    ret@[_, _, _, _] -> return ret
                    _ -> fail $ "Could not parse msg: " ++ (T.unpack msg)
    parseType (T.stripPrefix "counter" -> Just _) = return Counter
    parseType (T.stripPrefix "absolute" -> Just _) = return Absolute
    parseType _type = fail $ "Could not parse type: " ++ (T.unpack _type)
    parseValue value = case reads (T.unpack value) of
                            [(value', _)] -> return value'
                            _ -> fail $ "Could not parse value: " ++ (T.unpack value)
    parseAggregate (T.stripPrefix "average" -> Just _) = return Average
    parseAggregate (T.stripPrefix "max" -> Just _) = return Max
    parseAggregate (T.stripPrefix "min" -> Just _) = return Min
    parseAggregate (T.stripPrefix "sum" -> Just _) = return Sum
    parseAggregate aggregate = fail $ "Could not parse aggregate: " ++ (T.unpack aggregate)
