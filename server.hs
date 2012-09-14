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
import Control.Monad.Reader
import System.Environment (getArgs, getEnv)
import System.FilePath.Posix ((</>))
import System.Directory
import Data.Maybe (maybe)

getSettings :: IO Settings
getSettings = do
  args <- getArgs
  dbdir <- case null args of
       True -> do
         home <- getEnv "HOME"
         return $ home </> "stats.rrd"
       False -> return $ head args
  stats <- atomically $ newTVar (M.empty)
  errorChan <- atomically $ newBroadcastTChan
  return $ Settings stats errorChan 4242 dbdir

main :: IO ()
main = do
  settings <- getSettings
  forkIO $ runStatskellT errorPrinter settings
  forkIO $ runStatskellT flusher settings
  sock <- socket AF_INET Datagram defaultProtocol
  bindSocket sock (SockAddrInet (port settings) iNADDR_ANY)
  handle <- socketToHandle sock ReadMode
  forever $ do
    msg <- TI.hGetLine handle
    -- Might be too small granularity. If this proofs to be a problem,
    -- create a worker, producer with channels
    forkIO $ runStatskellT (messageHandler msg) settings

errorPrinter :: Statskell IO ()
errorPrinter = do
  readChan <- asks errorChan >>= liftIO . atomically . dupTChan
  liftIO $ forever $ do
    err <- liftIO $ atomically $ readTChan readChan
    liftIO $ TI.hPutStrLn stderr err

flusher :: Statskell IO ()
flusher = ask >>= \settings -> liftIO $ forever $ do
  threadDelay 10000000
  values <- atomically $ do
    let state = stats settings
    oldMap <- readTVar state
    writeTVar state (M.map (const []) oldMap)
    return oldMap
  exportValues (databaseDir settings) values
  where
    rrdtool _ [] _ = return ()
    rrdtool rrdfile buckets values = createProcess (proc "rrdtool" ["updatev", rrdfile, "--template", (concat $ intersperse ":" buckets), (concat $ intersperse ":" ("N" : values))]) >> return ()
    consolidateValue [] = 0
    consolidateValue xs@(x:_) = case consolidate ^$ x of
                              Max -> foldr1 max [value ^$ stat | stat <- xs]
                              Min -> foldr1 min [value ^$ stat | stat <- xs]
                              Sum -> foldr1 (+) [value ^$ stat | stat <- xs]
                              Average -> average [value ^$ stat | stat <- xs] 0 0
    average [] n acc = acc / n
    average (!x:xs) !n !acc = average xs (n+1) (acc + x)
    exportValues path m = do
      let buckets = map T.unpack $ M.keys m
          values = [show (consolidateValue value) | value <- M.elems m]
      _ <- rrdtool path buckets values
      return ()

messageHandler :: Text -> Statskell IO ()
messageHandler msg = do
  echan <- asks errorChan
  state <- asks stats
  liftIO $ case parseMsg msg of
       Left err -> atomically $ writeTChan echan $ T.pack err
       Right stat -> atomically $ do
         modifyTVar state (M.insertWith' ((:) . head) (bucket ^$ stat) [stat])
