{-# LANGUAGE OverloadedStrings #-}
module M
  ( refresh
  , Restaurant (..)
  , Menu (..)
  , View (..)
  ) where

import Control.Concurrent.MVar
import Control.Lens
import Data.IORef
import Data.Thyme
import Data.Thyme.Calendar.WeekDate

import Config
import M.Einstein
import M.Wijkanders
import M.Types
import M.Karen
import Util

-- | Refreshes menus.
refresh :: Config
        -> IO (IORef View -- view model
              , MVar () -> IO ()) -- update view
refresh c = do
  date <- fmap (view _zonedTimeToLocalTime) getZonedTime
  ref <- newIORef (View [] "" date)
  return
    ( ref
    , \upd -> do
        takeMVar upd
        putStrLn "Upd"
        update c >>= writeIORef ref)

update :: Config -> IO View
update c = do
  dateNow <- fmap (view _zonedTimeToLocalTime) getZonedTime
  let (textday, date) =
        if (dateNow ^. (_localTimeOfDay . _todHour)) >= view cNextDayHour c
          then ( "Tomorrow"
               , dateNow & (_localDay . gregorian . _ymdDay) %~ (+ 1))
          else ("Today", dateNow)
  let day = date ^. _localDay
  let weekday = (date ^. (_localDay . mondayWeek . _mwDay)) - 1
  rest <-
    sequence
      [ getKaren day "K\229rrestaurangen" johannebergLunch <$> safeGetBS karen
      , getKarenToday "Linsen" johannebergLunch <$> safeGetBS linsenToday
      , getEinstein weekday <$> safeGet "http://butlercatering.se/einstein"
      , getKaren day "L's Kitchen" lindholmenLunch <$> safeGetBS ls
      , getKaren day "Xpress" johannebergLunch <$> safeGetBS xpress
      , getWijkanders (weekday + 1) <$> safeGet "http://www.wijkanders.se/restaurangen/"
      , getKaren day "S.M.A.K." johannebergLunch <$> safeGetBS smak
      ]
  return
    (View rest textday date)
  where
    -- Restaurant api links
    karen = "http://carboncloudrestaurantapi.azurewebsites.net/api/menuscreen/getdataweek?restaurantid=5"
    linsenToday = "http://carboncloudrestaurantapi.azurewebsites.net/api/menuscreen/getdataday?restaurantid=33"
    xpress = "http://carboncloudrestaurantapi.azurewebsites.net/api/menuscreen/getdataweek?restaurantid=7"
    ls = "http://carboncloudrestaurantapi.azurewebsites.net/api/menuscreen/getdataweek?restaurantid=8"
    smak = "http://carboncloudrestaurantapi.azurewebsites.net/api/menuscreen/getdataweek?restaurantid=42"

    -- Restaurant menu links
    johannebergLunch = "https://chalmerskonferens.se/lunchmenyer-johanneberg/"
    lindholmenLunch = "https://chalmerskonferens.se/lunchmenyer-lindholmen/"
