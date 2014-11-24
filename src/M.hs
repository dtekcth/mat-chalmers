{-# LANGUAGE OverloadedStrings #-}
module M 
  ( refresh
  , Restaurant (..)
  , Menu (..)
  ) where

import Control.Concurrent (forkIO, threadDelay)
import Control.Exception
import Control.Monad
import Data.IORef
import Data.Maybe
import qualified Data.Text.Lazy as T
import Data.Text.Lazy.Encoding (decodeUtf8)
import Data.Time.Clock
import Data.Time.Format
import System.Locale
import Network.HTTP.Conduit
import Text.HTML.TagSoup


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
refresh :: IO (IORef [Restaurant])
refresh = do
  ref <- newIORef []
  forkIO . forever $ do
    update >>= writeIORef ref
    threadDelay (1000000 * 60 * 60)
  return ref
 where
  update = do
    karen <- mapM (uncurry getKaren)
      [ ("Linsen", "http://cm.lskitchen.se/johanneberg/linsen/sv/%F.rss")
      , ("Kårrestaurangen", "http://cm.lskitchen.se/johanneberg/karrestaurangen/sv/%F.rss")
      ]
    einstein <- getEinstein
    let rest = catMaybes (karen ++ [einstein])
    return rest

-- | Get a restaurant that kåren has.
getKaren :: T.Text -> String -> IO (Maybe Restaurant)
getKaren name format = do
  url <- liftM (formatTime defaultTimeLocale format) getCurrentTime
  handle' (const (return Nothing)) $ do
    rss <- simpleHttp url
    let doc = parseTags (decodeUtf8 rss)
        items = partitions (~== ss "<item>") doc
        lunch = contentOf "<title>"
        spec = T.takeWhile (/= '@') . contentOf "<description>"
        menus = map (\i -> Menu (lunch i) (spec i)) items
    return . Just $ Restaurant name menus

-- | Get Einstein menu
getEinstein :: IO (Maybe Restaurant)
getEinstein = handle' (const (return Nothing)) $ do
  menuSite <- simpleHttp "http://butlercatering.se/einstein"
  dayOfWeek <- liftM (pred . read . formatTime defaultTimeLocale "%w") getCurrentTime
  let tags = parseTags (decodeUtf8 menuSite)
      days = partitions (~== ss "<div class=\"field-day\">") tags
      menus = map (take 2 . map (head . drop 1) . partitions (~== ss "<p>")) days
      menus' = map (map (Menu "Lunch" . getTT)) menus
  return . Just . Restaurant "Einstein" $ fromMaybe [] (menus' !!? dayOfWeek)

contentOf :: String -> [Tag T.Text] -> T.Text
contentOf tag = maybe "" getTT . (!!? 1) . head . sections (~== ss tag)

getTT :: Tag T.Text -> T.Text
getTT (TagText t) = t
getTT _ = ""

-- | To force type to be String
ss :: String -> String
ss = id

-- | Handler for HttpExceptions
handle' :: (HttpException -> IO a) -> IO a -> IO a
handle' = handle

-- | Safe list index
(!!?) :: [a] -> Int -> Maybe a
[]     !!? _ = Nothing
(a:as) !!? n
  | n < 0 = Nothing
  | n == 0 = Just a
  | otherwise = as !!? pred n
