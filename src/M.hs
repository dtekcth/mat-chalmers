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
import Data.String (fromString)
import Data.Thyme
import Data.Thyme.Calendar.WeekDate
import System.Locale (defaultTimeLocale)

import Config
import M.Einstein
import M.Internal
import M.Karen

-- | Refreshes menus.
refresh :: Config -> IO (IORef View, MVar () -> IO ())
refresh c = do
  ref <- newIORef (View [] "")
  return
    ( ref
    , \upd -> do
        takeMVar upd
        putStrLn "Upd"
        update c >>= writeIORef ref)

update :: Config -> IO View
update c = do
  dateNow <- fmap (view _zonedTimeToLocalTime) getZonedTime
  let (day,date) =
           if (dateNow ^. (_localTimeOfDay . _todHour)) >= nextDayHour c
              then ("Tomorrow",dateNow & (_localDay . gregorian . _ymdDay) %~ (+ 1))
              else ("Today",dateNow)
  let weekday = (date ^. (_localDay . mondayWeek . _mwDay)) - 1
  rest <-
    fmap
      catMaybes
      (sequence
         [ getKaren weekday "K\229rrestaurangen" karen
         , getKaren weekday "Linsen" linsen
         , getEinstein weekday
         , getKaren weekday "L's Kitchen" ls
         , getKaren weekday "Xpress" xpress
         ])
  return (View rest (fromString (day ++ (formatTime defaultTimeLocale " / %F" date))))
  where
    karen =
      "http://intern.chalmerskonferens.se/view/restaurant/karrestaurangen/Veckomeny.rss"
    ls =
      "http://intern.chalmerskonferens.se/view/restaurant/l-s-kitchen/Projektor.rss"
    linsen =
      "http://intern.chalmerskonferens.se/view/restaurant/linsen/RSS%20Feed.rss"
    xpress =
      "http://intern.chalmerskonferens.se/view/restaurant/express/Vänster.rss"
