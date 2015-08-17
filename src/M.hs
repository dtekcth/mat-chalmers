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

import Config
import M.Einstein
import M.Karen
import M.Internal



-- | Refreshes menus.
refresh :: Config -> IO (IORef View, MVar () -> IO ())
refresh c =
  do ref <- newIORef (View [] "")
     return (ref, \upd -> do takeMVar upd
                             putStrLn "Upd"
                             update c >>= writeIORef ref)

update :: Config -> IO View
update c =
  do dateNow <-
       fmap (view _zonedTimeToLocalTime) getZonedTime
     let (tomorrow,date) =
           if (dateNow ^.
               (_localTimeOfDay . _todHour)) >=
              nextDayHour c
              then (True
                   ,dateNow &
                    (_localDay . gregorian . _ymdDay) %~
                    (+ 1))
              else (False,dateNow)
     rest <-
       fmap catMaybes
            (sequence [getKaren date
                                "K\229rrestaurangen"
                                "http://intern.chalmerskonferens.se/view/restaurant/karrestaurangen/Veckomeny.rss?today=true"
                      ,getKaren date "Linsen" "http://cm.lskitchen.se/johanneberg/linsen/sv/%F.rss"
                      ,getEinstein date
                      ,getKaren date
                                "L's Kitchen"
                                "http://cm.lskitchen.se/lindholmen/foodcourt/sv/%F.rss"])
     return (View rest
                  (if tomorrow
                      then "Tomorrow"
                      else "Today"))
