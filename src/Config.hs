{-# LANGUAGE OverloadedStrings #-}
-- | Configuration

module Config
  ( Config()
  , nextDayHour
  , updateInterval
  , config
  ) where

-- | Configuration record
data Config =
  Config { nextDayHour :: Int -- ^ When to show next days menu
         , updateInterval :: Int -- ^ Time to wait between menu updates
         }

-- | Default configuration
config :: Config
config =
  Config 14
         (1000000 * 60 * 60)
