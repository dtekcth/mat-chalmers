{-# LANGUAGE OverloadedStrings #-}
module Main where


import Control.Concurrent (forkIO, threadDelay)
import Control.Monad
import Control.Monad.Trans (liftIO)
import Data.IORef
import Data.Time.Clock
import Data.Time.Format
import qualified Data.Text.Lazy as T
import Data.Text.Lazy.Encoding (decodeUtf8)
import Network.HTTP.Conduit (simpleHttp)
import System.Locale
import Web.Scotty
import Text.HTML.TagSoup

data Restaurant = Restaurant
  { name :: T.Text
  , menu :: [Menu]
  } deriving (Eq, Show)

data Menu = Menu
  { lunch :: T.Text
  , spec :: T.Text
  } deriving (Eq, Show)

main = scotty 3000 $ do
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

getRest :: T.Text -> String -> IO Restaurant
getRest name format = do
  url <- liftM (formatTime defaultTimeLocale format) getCurrentTime
  rss <- simpleHttp url
  let doc = parseTags (decodeUtf8 rss)
      items = partitions (~== ("<item>" :: String)) doc
      title is = (!! 1) . head . sections (~== ("<title>" :: String)) $ is
      desc is = (!! 1) . head . sections (~== ("<description>" :: String)) $ is
      menus = map (\i -> Menu (getTT (title i)) (getTT (desc i))) items
  return $ Restaurant name menus
 where
  getTT (TagText t) = t
  getTT _ = ""
