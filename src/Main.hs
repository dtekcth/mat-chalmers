{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE DeriveDataTypeable #-}
module Main (main) where

import Control.Applicative
import Control.Monad.Trans (liftIO)
import Data.Data
import Data.IORef
import qualified Data.Text.Lazy as T
import Data.Time.Clock
import Data.Time.Format
import System.Locale
import Text.Hastache
import Text.Hastache.Context
import Web.Scotty
import Paths_mat_chalmers

import M

-- | What to pass to template.
data View = View
  { restaurants :: [Restaurant]
  , date :: T.Text
  } deriving (Eq, Show, Data, Typeable)


main = scotty 5007 $ do
  template <- liftIO $ getDataFileName "data/template.mustache"
  icon <- liftIO $ getDataFileName "data/icon.png"
  rref <- liftIO refresh
  get "/icon.png" (file icon)
  get "/" $ do
    rests <- liftIO $ readIORef rref
    time <- (T.pack . formatTime defaultTimeLocale "%F") <$> liftIO getCurrentTime
    let v = View rests time
    site <- liftIO $ hastacheFile defaultConfig template (mkGenericContext v)
    html site
