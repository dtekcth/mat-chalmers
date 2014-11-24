{-# LANGUAGE OverloadedStrings #-}
module Main (main) where

import Control.Applicative
import Control.Monad.Trans (liftIO)
import Data.IORef
import qualified Data.Text.Lazy as T
import Data.Time.Clock
import Data.Time.Format
import System.Locale
import Web.Scotty
import Network.Wai.Middleware.Static

import Paths_mat_chalmers

import M
import V (render, View (..))

main = scotty 5007 $ do
  rref <- liftIO refresh
  staticDir <- liftIO (getDataFileName "static")
  middleware $ staticPolicy (noDots >-> addBase staticDir)
  get "/" $ do
    rests <- liftIO $ readIORef rref
    time <- (T.pack . formatTime defaultTimeLocale "%F") <$> liftIO getCurrentTime
    let v = View rests time
    let site = render v
    html site
