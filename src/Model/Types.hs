{-# LANGUAGE TemplateHaskell, BangPatterns #-}

-- | Types and internal functions

module Model.Types where

import           Data.ByteString.Lazy                     ( ByteString )
import           Data.Text.Lazy                           ( Text )
import           Data.Thyme                               ( LocalTime )
import           Lens.Micro.Platform

-- | What to pass to template.
data View = View
  { restaurants :: [Restaurant]
  , day         :: Text
  , date        :: LocalTime
  } deriving (Show)

-- | One pretty restaurant.
data Restaurant = Restaurant
  { name :: Text
  , url  :: Text
  , menu :: Either NoLunch [Lunch]
  } deriving (Show)

data Menu = Menu
  { _mMonday    :: Either NoLunch [Lunch]
  , _mTuesday   :: Either NoLunch [Lunch]
  , _mWednesday :: Either NoLunch [Lunch]
  , _mThursday  :: Either NoLunch [Lunch]
  , _mFriday    :: Either NoLunch [Lunch]
  } deriving (Show)

data NoLunch
  = NoLunch
  | NMParseError String ByteString -- ^ The parse error. The string we tried to parse.
  deriving (Eq, Show)

-- | Menu of a restaurant.
-- Title, Body text
data Lunch = Lunch
  { _lName :: Text
  , _lFood :: Text
  }
  deriving (Eq, Show)

makeLenses ''Menu
makeLenses ''Lunch
