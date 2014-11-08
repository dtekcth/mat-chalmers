{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE DeriveDataTypeable #-}
module M 
  ( refresh
  , View (..)
  , Restaurant (..)
  , Menu (..)
  ) where

import Control.Concurrent (forkIO, threadDelay)
import Control.Monad
import Control.Monad.Trans (liftIO)
import Data.Data
import Data.Typeable
import Data.IORef
import qualified Data.Text.Lazy as T
import qualified Data.Text.Lazy.IO as T
import Data.Text.Lazy.Encoding (decodeUtf8)
import Data.Time.Clock
import Data.Time.Format
import Network.HTTP.Conduit (simpleHttp)
import System.Locale
import Text.HTML.TagSoup


-- | What to pass to template.
data View = View
  { restaurants :: [Restaurant]
  , date :: T.Text
  } deriving (Eq, Show, Data, Typeable)

-- | One pretty restaurant.
data Restaurant = Restaurant
  { name :: T.Text
  , menu :: [Menu]
  } deriving (Eq, Show, Data, Typeable)

-- | Menu of a restaurant.
data Menu = Menu
  { lunch :: T.Text
  , spec :: T.Text
  } deriving (Eq, Show, Data, Typeable)

-- | Refreshes menus hourly.
refresh :: IO (IORef View)
refresh = do
  ref <- newIORef (View [] "")
  forkIO . forever $ do
    update >>= writeIORef ref
    threadDelay (1000000 * 60 * 60)
  return ref
 where
  update = do
    rest <- mapM (uncurry getKaren)
      [ ("Linsen", "http://cm.lskitchen.se/johanneberg/linsen/sv/%F.rss")
      , ("Kårrestaurangen", "http://cm.lskitchen.se/johanneberg/karrestaurangen/sv/%F.rss")
      ]
    date <- liftM (T.pack . formatTime defaultTimeLocale "%F") getCurrentTime
    return (View rest date)

-- | Get a restaurang that kåren has.
getKaren :: T.Text -> String -> IO Restaurant
getKaren name format = do
  url <- liftM (formatTime defaultTimeLocale format) getCurrentTime
  rss <- simpleHttp url
  let doc = parseTags (decodeUtf8 rss)
      items = partitions (~== ss "<item>") doc
      lunch = contentOf "<title>"
      spec = T.takeWhile (/= '@') . contentOf "<description>"
      menus = map (\i -> Menu (lunch i) (spec i)) items
  return $ Restaurant name menus
 where

contentOf tag = getTT . (!! 1) . head . sections (~== ss tag)
 where
  getTT (TagText t) = t
  getTT _ = ""


-- | To force type to be String
ss :: String -> String
ss = id
