{-# LANGUAGE OverloadedStrings #-}
module Main (main) where

import Control.Concurrent
import Control.Monad
import Control.Monad.Trans (liftIO)
import Data.IORef
import Network.Wai.Middleware.Static
import Network.Wai.Middleware.RequestLogger
import Web.Scotty

import Paths_mat_chalmers

import Config
import M
import V (render)

main :: IO ()
main = do
  upd <- liftIO (newMVar ()) -- putMVar when to update
  (view, refreshAction) <- liftIO (refresh config)

  -- update thread
  forkIO . forever $ refreshAction upd

  -- timer thread
  forkIO . forever $
    tryPutMVar upd () >> threadDelay (updateInterval config)

  -- serve webpage
  serve view upd

serve :: IORef View -- View model
      -> MVar () -- Update signal
      -> IO ()
serve view upd = do
  staticDir <- getDataFileName "static"
  scotty (servePort config) $ do
    middleware (staticPolicy (noDots >-> addBase staticDir))
    middleware logStdout
    get "/" site
    get "/r" (liftIO (tryPutMVar upd ()) >> redirect "/") -- force update
  where
    site = (html . render) =<< liftIO (readIORef view)
