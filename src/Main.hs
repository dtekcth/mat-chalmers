{-# LANGUAGE OverloadedStrings #-}
module Main (main) where

import Control.Monad.Trans (liftIO)
import Data.IORef
import Text.Hastache
import Text.Hastache.Context
import Web.Scotty
import Paths_mat_chalmers

import M

main = scotty 5007 $ do
  template <- liftIO $ getDataFileName "template.mustache"
  icon <- liftIO $ getDataFileName "icon.png"
  rref <- liftIO refresh
  get "/icon.png" (file icon)
  get "/" $ do
    v <- liftIO $ readIORef rref
    site <- liftIO $ hastacheFile defaultConfig template (mkGenericContext v)
    html site
