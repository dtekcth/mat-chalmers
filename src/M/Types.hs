-- | Types and internal functions

module M.Types where

import Data.Text.Lazy (Text)
import qualified Data.Text.Lazy as T
import Data.Thyme

-- | What to pass to template.
data View = View
  { restaurants :: [Restaurant]
  , day :: T.Text
  , date :: LocalTime
  } deriving (Eq, Show)

-- | One pretty restaurant.
data Restaurant = Restaurant
  { name :: T.Text
  , url :: T.Text
  , menu :: Either NoMenu [Menu]
  } deriving (Eq, Show)

data NoMenu
  = NoLunch
  | SomethingWrong
  deriving (Eq, Show)

-- | Menu of a restaurant.
data Menu = Menu Text Text
  deriving (Eq, Show)
