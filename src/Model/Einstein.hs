-- | Get daily menu for Einstein
module Model.Einstein where

import           Data.Text.Lazy (Text)
import qualified Data.Text.Lazy as T
import           GHC.Exts
import           Text.HTML.TagSoup

import           Model.Types hiding (menu, date)
import           Util

myurl :: String
myurl = "http://butlercatering.se/einstein"

myname :: Text
myname = fromString "Einstein"

-- | Get Einstein menu
getEinstein :: Int -> Maybe Text -> Restaurant
getEinstein weekday text =
  Restaurant myname (fromString myurl) .
  maybe (Left NoLunch) Right $
  getMenus weekday =<< text

getMenus :: Int -> T.Text -> Maybe [Menu] -- Restaurant
getMenus weekday tags = do
  let parts = partitions (~== "<div class='field-day'>") (parseTags tags)
  day <- safeIdx parts weekday
  return $ menus day

menus :: [Tag T.Text] -> [Menu]
menus day =
  take 4 .
  filter (\(Menu _ spec) -> not $ T.null spec) .
  map menu . partitions (~== "<p>") $
  day

menu :: [Tag T.Text] -> Menu
menu spec =
  fixup $ Menu (fromString "Lunch") (T.strip . innerText . takeNext $ spec)

fixup :: Menu -> Menu
fixup m@(Menu _ spec)
  | Just suf <- T.stripPrefix (fromString "Veg:") spec =
      Menu (fromString "Vegetarisk") (T.strip suf)
  | otherwise = m
