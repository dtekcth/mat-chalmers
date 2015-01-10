{-# LANGUAGE OverloadedStrings #-}
-- | Configuration

module Config
  (Config()
  ,nextDayHour
  ,updateInterval
  ,rssFeeds
  ,def) where

import Data.Text.Lazy as T

-- | Configuration record
data Config =
  Config {nextDayHour :: Int -- ^ When to show next days menu
         ,updateInterval :: Int -- ^ Time to wait between menu updates
         ,rssFeeds :: [(T.Text, String)] -- ^ RSS feeds for menus
         }

-- | Default configuration
def :: Config
def =
  Config 12
         (1000000 * 60 * 60) -- 1 hour
         [("Linsen","http://cm.lskitchen.se/johanneberg/linsen/sv/%F.rss")
         ,("K\229rrestaurangen" ,"http://cm.lskitchen.se/johanneberg/karrestaurangen/sv/%F.rss")]
