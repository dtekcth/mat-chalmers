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
import           Data.Time.Calendar
import           Data.Time.Calendar.WeekDate
import           Data.Time.Format
import           Data.Time.LocalTime
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
          do dateNow <- getZonedTime
             let (tomorrow, date) = maybeAddDay dateNow
             karen <- mapM (uncurry (getKaren date)) restaurants
             einstein <- getEinstein date
             let rest =
                   catMaybes (karen ++
                              [einstein])
             return (View rest (if tomorrow
                                   then "Imorgon"
                                   else "Idag"))
        maybeAddDay :: ZonedTime -> (Bool, ZonedTime)
        maybeAddDay zt@(ZonedTime lt tz) = if hour > 16
                                              then (True, newZt)
                                              else (False, zt)
          where hour = todHour (localTimeOfDay lt)
                newZt = ZonedTime lt' tz
                tod = localTimeOfDay lt
                ld = localDay lt
                lt' = LocalTime (addDays 1 ld) tod
        restaurants =
          [("Linsen","http://cm.lskitchen.se/johanneberg/linsen/sv/%F.rss")
          ,("K\229rrestaurangen"
           ,"http://cm.lskitchen.se/johanneberg/karrestaurangen/sv/%F.rss")]

-- | Get a restaurant that kåren has.
getKaren :: ZonedTime -> T.Text -> String -> IO (Maybe Restaurant)
getKaren date name format =
  handle' (liftM getRestaurant (getAndParse url))
  where url = (formatTime defaultTimeLocale format) date
        getRestaurant tags = Restaurant name menus
          where items = partitions (~== ss "<item>") tags
                lunch = contentOf "<title>"
                spec = T.takeWhile (/= '@') . contentOf "<description>"
                menus = map (\i -> Menu (lunch i) (spec i)) items

-- | Get Einstein menu
getEinstein :: ZonedTime -> IO (Maybe Restaurant)
getEinstein date =
  handle' (liftM getRestaurant (getAndParse "http://butlercatering.se/einstein"))
  where (_, _, weekday) = toWeekDate (localDay (zonedTimeToLocalTime date))
        getRestaurant tags = Restaurant "Einstein" menus''
          where days = partitions (~==  ss "<div class=\"field-day\">") tags
                menus = map (take 2 . map (!! 1) . partitions (~== ss "<p>")) days
                menus' = map (map (Menu "Lunch" . getTT)) menus
                menus'' = fromMaybe [] (menus' !!? weekday)


-- UTILITIES

contentOf :: String -> [Tag T.Text] -> T.Text
contentOf tag = maybe "" getTT . (!!? 1) . head . sections (~== ss tag)

getTT :: Tag T.Text -> T.Text
getTT (TagText t) = t
getTT _ = ""

-- | Fetch url and parse to tagsoup
getAndParse :: String -> IO [Tag T.Text]
getAndParse url = liftM (parseTags . decodeUtf8) (simpleHttp url)

-- | To force type to be String
ss :: String -> String
ss = id

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
