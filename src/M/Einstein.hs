{-# LANGUAGE OverloadedStrings #-}

-- | Get daily menu for Einstein
module M.Einstein where

import           Control.Lens
import           Data.Maybe
import qualified Data.Text.Lazy as T
import           Data.Thyme
import           Data.Thyme.Calendar.WeekDate
import           Text.Taggy
import           Text.Taggy.Lens as TT

import           M.Internal hiding (menu)

-- | Get Einstein menu
getEinstein :: LocalTime -> IO (Maybe Restaurant)
getEinstein date =
  handle' (fmap (getRestaurant weekday)
                (get "http://butlercatering.se/einstein"))
  where weekday =
          date ^. (_localDay . mondayWeek . _mwDay)

getRestaurant :: Int -> T.Text -> Restaurant
getRestaurant weekday tags =
  fromMaybe (Restaurant "Einstein" [])
            (days !!? (weekday - 1))
  where days =
          tags ^..
          html .
          allAttributed (ix "class" . only "field-day") .
          TT.children .
          to menus

menus :: [Node] -> Restaurant
menus day = Restaurant "Einstein" (day ^.. each . allNamed (only "p") . to menu)

menu :: Element -> Menu
menu spec = Menu "Lunch" (T.fromStrict (innerText spec))
