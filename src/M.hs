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
  let weekday = (date ^. (_localDay . mondayWeek . _mwDay))
  rest <-
    fmap catMaybes $
    sequence
      [ getKaren weekday "K\229rrestaurangen" karen karenl
      , getKaren weekday "Linsen" linsen linsenl
      , getEinstein weekday
      , getKaren weekday "L's Kitchen" ls lsl
      , getKaren weekday "Xpress" xpress xpressl
      , getWijkanders weekday
      ]
  return
    (View rest day date)
  where
    karen =
      "http://intern.chalmerskonferens.se/view/restaurant/karrestaurangen/Veckomeny.rss"
    karenl =
      "http://chalmerskonferens.se/restauranger/johanneberg/karrestaurangen/"
    ls =
      "http://intern.chalmerskonferens.se/view/restaurant/l-s-kitchen/Projektor.rss"
    lsl = "http://chalmerskonferens.se/restauranger/lindholmen/ls-kitchen/"
    linsen =
      "http://intern.chalmerskonferens.se/view/restaurant/linsen/RSS%20Feed.rss"
    linsenl =
      "http://chalmerskonferens.se/restauranger/johanneberg/restaurangcafe-linsen/"
    xpress =
      "http://intern.chalmerskonferens.se/view/restaurant/express/Vänster.rss"
    xpressl = "http://chalmerskonferens.se/restauranger/johanneberg/express/"
