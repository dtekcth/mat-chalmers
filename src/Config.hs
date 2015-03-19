{-# LANGUAGE OverloadedStrings #-}
-- | Configuration

module Config
  ( Config()
  , nextDayHour
  , updateInterval
  , rssFeeds
  , config
  ) where

import Data.Text.Lazy as T

-- | Configuration record
data Config =
  Config { nextDayHour :: Int -- ^ When to show next days menu
         , updateInterval :: Int -- ^ Time to wait between menu updates
         , rssFeeds :: [(T.Text, String)] -- ^ RSS feeds for menus
         }

-- | Default configuration
config :: Config
config =
  Config 14
         (1000000 * 60 * 60)
         [ ("Linsen", "http://cm.lskitchen.se/johanneberg/linsen/sv/%F.rss")
         , ("K\229rrestaurangen", "http://cm.lskitchen.se/johanneberg/karrestaurangen/sv/%F.rss")
         ]
