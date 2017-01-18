{-# LANGUAGE OverloadedStrings #-}
-- | Configuration

module Config where

import Data.Maybe

-- | Configuration record
data Config =
  Config (Maybe Int) (Maybe Int) (Maybe Int)
  deriving (Show,Read)



nextDayHour :: Config -> Int -- ^ When to show next days menu
nextDayHour (Config i _ _) = fromMaybe 14 i
updateInterval :: Config -> Int -- ^ Time to wait between menu updates
updateInterval (Config _ i _) = 1000000 * fromMaybe (60 * 30) i
servePort :: Config -> Int -- ^ Port to run web server on
servePort (Config _ _ p) = fromMaybe 5007 p

-- | Default configuration
config :: Config
config =
  Config Nothing
         Nothing
         Nothing
