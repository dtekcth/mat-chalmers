-- | Types and internal functions

module Model.Types where

import Data.Text.Lazy (Text)
import Data.Thyme

-- | What to pass to template.
data View = View
  { restaurants :: [Restaurant]
  , day :: Text
  , date :: LocalTime
  } deriving (Eq, Show)

-- | One pretty restaurant.
data Restaurant = Restaurant
  { name :: Text
  , url :: Text
  , menu :: Either NoMenu [Menu]
  } deriving (Eq, Show)

data NoMenu
  = NoLunch
  | SomethingWrong (Maybe Text)
  deriving (Eq, Show)

-- | Menu of a restaurant.
data Menu = Menu Text Text
  deriving (Eq, Show)
