{-# LANGUAGE OverloadedStrings #-}

-- | Get daily menu for Einstein
module M.Einstein where

import           Control.Lens
import           Data.Maybe
import qualified Data.Text.Lazy as LT
import           Data.Thyme
import           Data.Thyme.Calendar.WeekDate
import           Text.Taggy
import           Text.Taggy.Lens as TT

import           M.Internal hiding (menu, spec, date)

-- | Get Einstein menu
getEinstein :: LocalTime -> IO (Maybe Restaurant)
getEinstein date =
  handle' (fmap (getRestaurant weekday)
                (get "http://butlercatering.se/einstein"))
  where weekday =
          date ^. (_localDay . mondayWeek . _mwDay)

getRestaurant :: Int -> LT.Text -> Restaurant
getRestaurant weekday tags =
  fromMaybe (Restaurant "Einstein" []) day
  where day =
          (tags ^.. html
           . allAttributed (ix "class" . only "field-day")
           . TT.children
           . to menus)
          ^? ix (weekday - 1)

menus :: [Node] -> Restaurant
menus day =
  Restaurant
    "Einstein"
    (day ^.. each
     . allNamed (only "p")
     . to menu)

menu :: Element -> Menu
menu spec = Menu "Lunch" (LT.fromStrict (spec ^. contents))
