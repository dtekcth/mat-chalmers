-- | Get daily menu for Einstein
module M.Einstein where

import qualified Data.Text.Lazy as T
import           GHC.Exts
import           Text.HTML.TagSoup

import           M.Internal hiding (menu, date)
import           Util

-- | Get Einstein menu
getEinstein :: Int -> IO (Maybe Restaurant)
getEinstein weekday = do
  text <- handle' (get "http://butlercatering.se/einstein")
  return (text >>= getRestaurant weekday)

getRestaurant :: Int -> T.Text -> Maybe Restaurant
getRestaurant weekday tags =
  do let parts = partitions (~== "<div class='field-day'>") (parseTags tags)
     days <- safeIdx parts weekday
     return (menus days)

-- menus :: [Node] -> Restaurant
menus day =
  Restaurant
    (fromString "Einstein")
    (take 4 .
     filter (not . T.null . spec) . map veg . map menu . partitions (~== "<p>") $
     day)

veg m@(Menu _ spec)
  | Just suf <- T.stripPrefix (fromString "Veg:") spec =
      Menu (fromString "Vegetarisk") (T.strip suf)
  | otherwise = m

-- menu :: Element -> Menu
menu spec = Menu (fromString "Lunch") (T.strip . innerText . takeNext $ spec)
