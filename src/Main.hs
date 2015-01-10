{-# LANGUAGE OverloadedStrings #-}
module Main (main) where

import Control.Monad.Trans (liftIO)
import Data.IORef
import Network.Wai.Middleware.Static
import Web.Scotty

import Paths_mat_chalmers

import M
import V (render)
import Config

main :: IO ()
main =
  scotty 5007
         (do rref <- liftIO (refresh def)
             staticDir <- liftIO (getDataFileName "static")
             middleware (staticPolicy (noDots >-> addBase staticDir))
             get "/" (site rref))
  where site rref = (do view <- liftIO (readIORef rref)
                        html (render view))
