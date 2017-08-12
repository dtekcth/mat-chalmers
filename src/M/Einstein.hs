-- | Get daily menu for Einstein
module M.Einstein where

import qualified Data.Text.Lazy as T
import           GHC.Exts
import           Text.HTML.TagSoup

import           M.Types hiding (menu, date)
import           Util

myurl :: String
myurl = "http://butlercatering.se/einstein"

myname :: T.Text
myname = fromString "Einstein"

-- | Get Einstein menu
getEinstein :: Int -> IO Restaurant
getEinstein weekday = do
  text <- handle' (get myurl)
  return $
    Restaurant myname (fromString myurl) . maybe (Left NoLunch) Right $
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
