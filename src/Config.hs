{-# LANGUAGE OverloadedStrings #-}
-- | Configuration

module Config
  ( Config()
  , nextDayHour
  , updateInterval
  , servePort
  , config
  ) where

-- | Configuration record
data Config =
  Config { nextDayHour :: Int -- ^ When to show next days menu
         , updateInterval :: Int -- ^ Time to wait between menu updates
         , servePort :: Int -- ^ Port to run web server on
         }
  deriving (Show,Read)

-- | Default configuration
config :: Config
config =
  Config 14
         (1000000 * 60 * 60)
         5007
