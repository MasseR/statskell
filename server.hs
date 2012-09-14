{-# Language BangPatterns #-}
module Main where

import Data.Text.Lazy (Text)
import Types
import Parser
import Network.Socket
import System.IO (IOMode (ReadMode), stderr)
import Control.Concurrent.STM
import Control.Concurrent
import qualified Data.Map as M
import qualified Data.Text.Lazy.IO as TI
import qualified Data.Text.Lazy as T
import Data.Lens.Common
import Control.Monad
import System.Process
import Data.List (intersperse)

main :: IO ()
main = do
  state <- atomically $ newTVar (M.empty)
  errorChan <- atomically $ newTChan
  forkIO $ errorPrinter errorChan
  forkIO $ flusher state
  sock <- socket AF_INET Datagram defaultProtocol
  bindSocket sock (SockAddrInet 4242 iNADDR_ANY)
  handle <- socketToHandle sock ReadMode
  forever $ do
    msg <- TI.hGetLine handle
    -- Might be too small granularity. If this proofs to be a problem,
    -- create a worker, producer with channels
    forkIO $ messageHandler state msg

errorPrinter :: ErrorChan -> IO ()
errorPrinter echan = do
  readChan <- atomically $ dupTChan echan
  forever $ do
    err <- atomically $ readTChan readChan
    TI.hPutStrLn stderr err

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
    rrdtool [] _ = return ()
    rrdtool buckets values = createProcess (proc "rrdtool" ["updatev", rrdfile, "--template", (concat $ intersperse ":" buckets), (concat $ intersperse ":" ("N" : values))]) >> return ()
    consolidateValue [] = 0
    consolidateValue xs@(x:_) = case consolidate ^$ x of
                              Max -> foldr1 max [value ^$ stat | stat <- xs]
                              Min -> foldr1 min [value ^$ stat | stat <- xs]
                              Sum -> foldr1 (+) [value ^$ stat | stat <- xs]
                              Average -> average [value ^$ stat | stat <- xs] 0 0
    average [] n acc = acc / n
    average (!x:xs) !n !acc = average xs (n+1) (acc + x)
    exportValues m = do
      let buckets = map T.unpack $ M.keys m
          values = [show (consolidateValue value) | value <- M.elems m]
      _ <- rrdtool buckets values
      return ()

messageHandler :: State -> Text -> IO ()
messageHandler state msg = do
  case parseMsg msg of
       Left err -> putStrLn err
       Right stat -> atomically $ do
         modifyTVar state (M.insertWith' ((:) . head) (bucket ^$ stat) [stat])
