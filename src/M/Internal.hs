-- | Types and internal functions

module M.Internal where

import           Control.Exception
import           Control.Monad
import qualified Data.Text.Lazy as T
import           Data.Text.Lazy.Encoding (decodeUtf8)
import           Network.HTTP.Conduit

-- | What to pass to template.
data View = View
  { restaurants :: [Restaurant]
  , date :: T.Text
  } deriving (Eq, Show)

-- | One pretty restaurant.
data Restaurant = Restaurant
  { name :: T.Text
  , menu :: [Menu]
  } deriving (Eq, Show)

-- | Menu of a restaurant.
data Menu = Menu
  { lunch :: T.Text
  , spec :: T.Text
  } deriving (Eq, Show)


get :: String -> IO T.Text
get url = liftM decodeUtf8 (simpleHttp url)

-- | Safe list index
(!!?) :: [a] -> Int -> Maybe a
[]     !!? _ = Nothing
(a:as) !!? n
  | n < 0 = Nothing
  | n == 0 = Just a
  | otherwise = as !!? pred n

-- | Handler for HttpExceptions
handle' :: IO a -> IO (Maybe a)
handle' a = handle handler (liftM Just a)
  where handler :: HttpException -> IO (Maybe a)
        handler _ = return Nothing
