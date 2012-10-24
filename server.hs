{-# Language BangPatterns #-}
{-# Language OverloadedStrings #-}
{-# Language ScopedTypeVariables #-}
module Main where

import Data.Serialize
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
import Data.Maybe (maybe, listToMaybe)
import System.IO.Error (catchIOError)
import qualified Store as S
import Prelude hiding (catch)
import Control.Exception (catch)
import Data.Time.Clock.POSIX
import qualified Data.ByteString as B

getSettings :: IO Settings
getSettings = do
  args <- getArgs
  dbdir <- case null args of
       True -> do
         home <- getEnv "HOME"
         return $ home </> "stats.rrd"
       False -> return $ head args
  stats' <- (fmap decode (B.readFile dbdir))  `catch` (\(err :: IOError) -> return $ Right S.empty)
  stats <- case stats' of
                Left err -> do
                  putStrLn "Could not decode dbdir"
                  atomically $ newTVar S.empty
                Right s -> atomically $ newTVar s
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
flusher = loop
  where
    loop :: Statskell IO ()
    loop = do
      settings <- ask
      liftIO $ threadDelay 10000000
      liftIO $ atomically $ modifyTVar (stats settings) (S.consolidateRRA)
      rra <- liftIO $ atomically $ readTVar (stats settings)
      liftIO $ B.writeFile (databaseDir settings) $ encode rra
      loop

messageHandler :: Text -> Statskell IO ()
messageHandler msg = do
  echan <- asks errorChan
  state <- asks stats
  now <- liftIO getPOSIXTime
  liftIO $ case parseCmd msg of
       Nothing -> atomically $ writeTChan echan $ "Could not parse message"
       Just (AddItem stat) -> atomically $ do
         modifyTVar state (S.addElement (_bucket stat) (floor $ realToFrac now) (_value stat))
       Just (AddArchive elements interval cons) ->
         atomically $ modifyTVar state (S.addArchive elements interval cons)
