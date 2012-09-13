{-# Language BangPatterns #-}
module Main where

import Data.Text.Lazy (Text)
import Types
import Parser
import Network.Socket
import System.IO (IOMode (ReadMode))
import Control.Concurrent.STM
import Control.Concurrent
import qualified Data.Map as M
import qualified Data.Text.Lazy.IO as TI
import qualified Data.Text.Lazy as T
import Data.Lens.Common
import Control.Monad

main :: IO ()
main = do
  state <- atomically $ newTVar (M.empty)
  forkIO $ flusher state
  sock <- socket AF_INET Datagram defaultProtocol
  bindSocket sock (SockAddrInet 4242 iNADDR_ANY)
  handle <- socketToHandle sock ReadMode
  forever $ do
    msg <- TI.hGetLine handle
    -- Might be too small granularity. If this proofs to be a problem,
    -- create a worker, producer with channels
    forkIO $ messageHandler state msg

flusher :: State -> IO ()
flusher state = forever $ do
  threadDelay 10000000
  values <- atomically $ do
    oldMap <- readTVar state
    writeTVar state (M.map (const []) oldMap)
    return oldMap
  exportValues values
  where
    rrdfile = "stats.rrd"
    aggregateValue [] = 0
    aggregateValue xs@(x:_) = case aggregate ^$ x of
                              Max -> foldr1 max [value ^$ stat | stat <- xs]
                              Min -> foldr1 min [value ^$ stat | stat <- xs]
                              Sum -> foldr1 (+) [value ^$ stat | stat <- xs]
                              Average -> average [value ^$ stat | stat <- xs] 0 0
    average [] n acc = acc / n
    average (!x:xs) !n !acc = average xs (n+1) (acc + x)
    exportValues values = forM_ (M.assocs values) $ \(key, value) -> do
      putStrLn $ (T.unpack key) ++ " " ++ (show $ aggregateValue value)

messageHandler :: State -> Text -> IO ()
messageHandler state msg = do
  case parseMsg msg of
       Left err -> putStrLn err
       Right stat -> atomically $ do
         modifyTVar state (M.insertWith' ((:) . head) (bucket ^$ stat) [stat])
