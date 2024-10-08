{-# LANGUAGE TemplateHaskell, BangPatterns #-}

-- | Configuration

module Config where

import           Data.Foldable                            ( foldl' )
import           Lens.Micro.Platform

-- | Configuration record
data Config = Config
  { _cHelp        :: !Bool
  , _cNextDayHour :: !Int
  , _cInterval    :: !Int
  , _cPort        :: !Int
  , _cLogPath     :: !String
  , _cLogAge      :: !Int
  }
  deriving (Eq, Show)

makeLenses ''Config

defaultConfig :: Config
defaultConfig = Config False 14 (1000000 * 60 * 30) 5007 "logs" 14

-- | Create a Config we can touch
--
-- Yes, this function implementation is overly cute. It uses a lens to update
-- the first field of the tuple by using defaultConfig as first argument to
-- every function in the list. The brain needs to turn inside out a couple of
-- times before it makes sense, so if you're in a hurry, look at the types.
--
-- TODO: Feel free to bikeshed the function name.
recreateConfig
  :: ([Config -> Config], [String], [String]) -> (Config, [String], [String])
recreateConfig = _1 %~ foldl' (flip id) defaultConfig
