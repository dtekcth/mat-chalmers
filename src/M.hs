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
import Data.Maybe
import Data.Thyme
import Data.Thyme.Calendar.WeekDate

import Config
import M.Einstein
import M.Wijkanders
import M.Internal
import M.Karen

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
  let (day, date) =
        if (dateNow ^. (_localTimeOfDay . _todHour)) >= view cNextDayHour c
          then ( "Tomorrow"
               , dateNow & (_localDay . gregorian . _ymdDay) %~ (+ 1))
          else ("Today", dateNow)
  let weekday = (date ^. (_localDay . mondayWeek . _mwDay)) - 1
  rest <-
    catMaybes <$> sequence
      [ getKaren weekday "K\229rrestaurangen" karen johannebergLunch
      , getKaren weekday "Linsen" linsen johannebergLunch
      , getEinstein weekday
      , getKaren weekday "L's Kitchen" ls lindholmenLunch
      , getKaren weekday "Xpress" xpress johannebergLunch
      , getWijkanders weekday
      ]
  return
    (View rest day date)
  where
    -- Restaurant api links
    karen = "http://carboncloudrestaurantapi.azurewebsites.net/api/menuscreen/getdataweek?restaurantid=5"
    linsen = "http://carboncloudrestaurantapi.azurewebsites.net/api/menuscreen/getdataweek?restaurantid=33"
    xpress = "http://carboncloudrestaurantapi.azurewebsites.net/api/menuscreen/getdataweek?restaurantid=9"
    ls = "http://carboncloudrestaurantapi.azurewebsites.net/api/menuscreen/getdataweek?restaurantid=8"

    -- Restaurant menu links
    johannebergLunch = "https://chalmerskonferens.se/lunchmenyer-johanneberg/"
    lindholmenLunch = "https://chalmerskonferens.se/lunchmenyer-lindholmen/"
