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
import System.Locale (defaultTimeLocale)

import Config
import M.Einstein
import M.Internal
import M.Karen



-- | Refreshes menus.
refresh :: Config -> IO (IORef View, MVar () -> IO ())
refresh c =
  do ref <- newIORef (View [] "")
     return (ref, \upd -> do takeMVar upd
                             putStrLn "Upd"
                             update c >>= writeIORef ref)

update :: Config -> IO View
update c =
  do dateNow <- fmap (view _zonedTimeToLocalTime) getZonedTime
     let (_tomorrow,date) =
           if (dateNow ^. (_localTimeOfDay . _todHour)) >= nextDayHour c
              then (True,dateNow & (_localDay . gregorian . _ymdDay) %~ (+ 1))
              else (False,dateNow)
     rest <-
       fmap catMaybes
            (sequence [getKaren date "K\229rrestaurangen" karen
                      ,getKaren date "Linsen" linsen
                      ,getEinstein date
                      ,getKaren date "L's Kitchen" ls
                      ,getKaren date "Xpress" xpress])
     return (View rest (formatTitle date))
  where karen =
          "http://intern.chalmerskonferens.se/view/restaurant/karrestaurangen/Veckomeny.rss?today=true"
        ls =
          "http://intern.chalmerskonferens.se/view/restaurant/l-s-kitchen/Projektor.rss?today=true"
        linsen =
          "http://intern.chalmerskonferens.se/view/restaurant/linsen/RSS%20Feed.rss?today=true"
        xpress =
          "http://intern.chalmerskonferens.se/view/restaurant/express/Vänster.rss?today=true"
        formatTitle date = fromString (formatTime defaultTimeLocale "Today / %F" date)
