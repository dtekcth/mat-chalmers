{-# LANGUAGE OverloadedStrings #-}
module M
  ( refresh
  , Restaurant (..)
  , Menu (..)
  , View (..)
  ) where

import           Control.Concurrent (forkIO, threadDelay)
import           Control.Exception
import           Control.Monad
import           Data.IORef
import           Data.Maybe
import qualified Data.Text.Lazy as T
import           Data.Text.Lazy.Encoding (decodeUtf8)
import           Data.Time.Clock
import           Data.Time.Format
import           Network.HTTP.Conduit
import           System.Locale
import           Text.HTML.TagSoup

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

-- | Refreshes menus hourly.
refresh :: IO (IORef View)
refresh =
  do ref <- newIORef (View [] "")
     (forkIO . forever) $
       do update >>= writeIORef ref
          threadDelay (1000000 * 60 * 60)
     return ref
  where update =
          do date <- getCurrentTime
             karen <- mapM (uncurry (getKaren date)) restaurants
             einstein <- getEinstein date
             let rest =
                   catMaybes (karen ++
                              [einstein])
             return (View rest "Idag")
        restaurants =
          [("Linsen","http://cm.lskitchen.se/johanneberg/linsen/sv/%F.rss")
          ,("K\229rrestaurangen"
           ,"http://cm.lskitchen.se/johanneberg/karrestaurangen/sv/%F.rss")]

-- | Get a restaurant that kåren has.
getKaren :: UTCTime -> T.Text -> String -> IO (Maybe Restaurant)
getKaren date name format =
  do let url = (formatTime defaultTimeLocale format) date
     handle' (do rss <- simpleHttp url
                 return (getRestaurant rss))
  where getRestaurant rss = Restaurant name menus
          where doc = parseTags (decodeUtf8 rss)
                items = partitions (~== ss "<item>") doc
                lunch = contentOf "<title>"
                spec = T.takeWhile (/= '@') . contentOf "<description>"
                menus = map (\i -> Menu (lunch i) (spec i)) items

-- | Get Einstein menu
getEinstein :: UTCTime -> IO (Maybe Restaurant)
getEinstein date =
  handle' (do menuSite <- simpleHttp "http://butlercatering.se/einstein"
              return (getRestaurant menuSite))
  where getRestaurant site = Restaurant "Einstein" menus''
          where dayOfWeek = (pred . read . formatTime defaultTimeLocale "%w") date
                tags = parseTags (decodeUtf8 site)
                days = partitions (~== ss "<div class=\"field-day\">") tags
                menus = map (take 2 . map (head . drop 1) . partitions (~== ss "<p>")) days
                menus' = map (map (Menu "Lunch" . getTT)) menus
                menus'' = fromMaybe [] (menus' !!? dayOfWeek)

contentOf :: String -> [Tag T.Text] -> T.Text
contentOf tag = maybe "" getTT . (!!? 1) . head . sections (~== ss tag)

getTT :: Tag T.Text -> T.Text
getTT (TagText t) = t
getTT _ = ""

-- | To force type to be String
ss :: String -> String
ss = id

-- | Handler for HttpExceptions
handle' :: IO a -> IO (Maybe a)
handle' a = handle handler (liftM Just a)
  where handler :: HttpException -> IO (Maybe a)
        handler _ = return Nothing

-- | Safe list index
(!!?) :: [a] -> Int -> Maybe a
[]     !!? _ = Nothing
(a:as) !!? n
  | n < 0 = Nothing
  | n == 0 = Just a
  | otherwise = as !!? pred n
