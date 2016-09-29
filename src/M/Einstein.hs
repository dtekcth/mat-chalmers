-- | Get daily menu for Einstein
module M.Einstein where

import           Control.Lens
import qualified Data.Text.Lazy as LT
import           Data.Thyme
import           Data.Thyme.Calendar.WeekDate
import           GHC.Exts
import           Text.HTML.TagSoup

import           M.Internal hiding (menu, date)

-- | Get Einstein menu
getEinstein :: LocalTime -> IO (Maybe Restaurant)
getEinstein date =
  handle' (fmap (getRestaurant weekday)
                (get "http://butlercatering.se/einstein"))
  where weekday =
          date ^. (_localDay . mondayWeek . _mwDay)

getRestaurant :: Int -> LT.Text -> Restaurant
getRestaurant weekday tags =
  menus
    ((partitions (~== "<div class='field-day'>") (parseTags tags)) !!
     (weekday - 1))

-- menus :: [Node] -> Restaurant
menus day =
  Restaurant
    (fromString "Einstein")
    (take 4 .
     filter (not . LT.null . spec) . map veg . map menu . partitions (~== "<p>") $
     day)

veg m@(Menu _ spec)
  | Just suf <- LT.stripPrefix (fromString "Veg:") spec =
      Menu (fromString "Vegetarisk") (LT.strip suf)
  | otherwise = m

-- menu :: Element -> Menu
menu spec = Menu (fromString "Lunch") (LT.strip . innerText . takeNext $ spec)

takeNext = take 1 . drop 1
