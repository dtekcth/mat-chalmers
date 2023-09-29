
-- | Types and internal functions

module Model.Types where

import           Data.ByteString.Lazy                     ( ByteString )
import           Data.Text.Lazy                           ( Text )
import           Data.Thyme                               ( LocalTime )

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
  , menu :: Either NoMenu [Menu]
  } deriving (Show)

data NoMenu
  = NoLunch
  | NMParseError String ByteString -- ^ The parse error. The string we tried to parse.
  deriving (Show)

-- | Menu of a restaurant.
-- Title, Body text
data Menu = Menu Text Text
  deriving (Eq, Show)
