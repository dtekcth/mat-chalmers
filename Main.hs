{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE DeriveDataTypeable #-}
module Main where

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
import Text.Hastache
import Web.Scotty

data Restaurant = Restaurant
  { name :: T.Text
  , menu :: [Menu]
  } deriving (Eq, Show, Data, Typeable)

data Menu = Menu
  { lunch :: T.Text
  , spec :: T.Text
  } deriving (Eq, Show, Data, Typeable)

main = scotty 5007 $ do
  template <- T.readFile "template.html"
  rref <- liftIO refresh
  get "/" $ do
    rs <- liftIO $ readIORef rref
    html $ T.concat
      [ "<pre>"
      , T.intercalate "\n\n" . map ppRestaurant $ rs
      , "</pre>"
      ]

ppRestaurant :: Restaurant -> T.Text
ppRestaurant (Restaurant name menus) = T.concat
  [ T.toUpper name
  , "\n"
  , T.intercalate "\n" $ map ppMenu menus
  ]

ppMenu :: Menu -> T.Text
ppMenu (Menu lunch stuff) = T.concat
  [ lunch
  , ": "
  , stuff
  ]

-- | Refreshes menus hourly.
refresh :: IO (IORef [Restaurant])
refresh = do
  ref <- newIORef []
  update >>= writeIORef ref
  forkIO . forever $ do
    forkIO (update >>= writeIORef ref)
    threadDelay (1000000 * 60 * 60)
  return ref

-- | Get menus.
update :: IO [Restaurant]
update = mapM (uncurry getRest)
  [ ("Linsen", "http://cm.lskitchen.se/johanneberg/linsen/sv/%F.rss")
  , ("Kårrestaurangen", "http://cm.lskitchen.se/johanneberg/karrestaurangen/sv/%F.rss")
  ]

-- | Get a restaurang that kåren has.
getRest :: T.Text -> String -> IO Restaurant
getRest name format = do
  url <- liftM (formatTime defaultTimeLocale format) getCurrentTime
  rss <- simpleHttp url
  let doc = parseTags (decodeUtf8 rss)
      items = partitions (~== ("<item>" :: String)) doc
      title = findContentOf "<title>"
      desc = T.takeWhile (/= '@') . findContentOf "<description>"
      menus = map (\i -> Menu (title i) (desc i)) items
  return $ Restaurant name menus
 where
  getTT (TagText t) = t
  getTT _ = ""
  findContentOf :: String -> [Tag T.Text] -> T.Text
  findContentOf tag = getTT . (!! 1) . head . sections (~== tag)
