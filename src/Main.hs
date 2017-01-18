{-# LANGUAGE OverloadedStrings #-}
module Main (main) where

import           Control.Applicative (optional)
import           Control.Concurrent
import           Control.Monad
import           Control.Monad.Trans (liftIO)
import           Data.IORef
import qualified Data.Text as T
import qualified Data.Text.IO as T
import           Descriptive
import           Descriptive.Options
import           Network.Wai.Middleware.RequestLogger
import           Network.Wai.Middleware.Static
import           System.Environment
import           Text.Read (readMaybe)
import           Web.Scotty hiding (options)

import           Paths_mat_chalmers

import           Config
import           M
import           V (render)

main :: IO ()
main = do
  args <- getArgs
  case consume options (map T.pack args) of
    Succeeded conf -> run conf
    Failed (Wrap (Stopped Help) _) -> do
      putStr "mat-chalmers "
      T.putStrLn $ textDescription (describe options [])
    _ -> run config

run conf = do
  upd <- liftIO (newMVar ()) -- putMVar when to update
  (view, refreshAction) <- liftIO (refresh conf)
  -- updater thread
  forkIO . forever $ refreshAction upd
  -- timer thread
  forkIO . forever $ tryPutMVar upd () >> threadDelay (updateInterval config)
  -- serve webpage
  serve conf view upd


data Stoppers = Help

options :: Monad m => Consumer [T.Text] (Option Stoppers) m Config
options = help *> (Config Nothing <$> interval <*> port)
  where
    help = stop (flag "help" "Show help" Help)
    interval =
      fmap (>>= (readMaybe . T.unpack))
        (optional (arg "interval" "Update interval in seconds"))
    port =
      fmap (>>= (readMaybe . T.unpack))
        (optional (arg "port" "Port to serve"))


serve :: Config
      -> IORef View -- View model
      -> MVar () -- Update signal
      -> IO ()
serve conf view upd = do
  staticDir <- getDataFileName "static"
  scotty (servePort conf) $ do
    middleware (staticPolicy (noDots >-> addBase staticDir))
    middleware logStdout
    get "/" site
    get "/r" (liftIO (tryPutMVar upd ()) >> redirect "/") -- force update
  where
    site = (html . render) =<< liftIO (readIORef view)
