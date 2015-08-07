{-# LANGUAGE OverloadedStrings #-}
module Main (main) where

import Control.Concurrent
import Control.Concurrent.MVar
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
main =
  do upd <- liftIO (newMVar ()) -- putMVar when to update
     (view,refreshAction) <- liftIO (refresh config)
     liftIO . forkIO . forever $ refreshAction upd
     liftIO . forkIO . forever $
       tryPutMVar upd () >> threadDelay (updateInterval config)
     staticDir <- liftIO (getDataFileName "static")
     scotty 5007
            (do middleware (staticPolicy (noDots >-> addBase staticDir))
                get "/" (site view)
                get "/r" (liftIO (tryPutMVar upd ()) >> redirect "/"))
  where site rref =
          (do view <- liftIO (readIORef rref)
              html (render view))
