{-# LANGUAGE OverloadedStrings #-}
module M
  ( refresh
  , Restaurant (..)
  , Menu (..)
  , View (..)
  ) where

import Control.Concurrent (forkIO, threadDelay)
import Control.Concurrent.MVar
import Control.Lens
import Control.Monad
import Data.IORef
import Data.Maybe
import Data.Thyme

import Config
import M.Einstein
import M.Karen
import M.Internal

-- | Refreshes menus.
refresh :: Config -> IO (IORef View, MVar () -> IO ())
refresh c  =
  do ref <- newIORef (View [] "")
     return (ref, \upd -> do takeMVar upd
                             putStrLn "Upd"
                             update c >>= writeIORef ref)

update :: Config -> IO View
update c =
  do dateNow <- fmap (view _zonedTimeToLocalTime) getZonedTime
     let (tomorrow, date) =
           if (dateNow ^. (_localTimeOfDay . _todHour)) >= nextDayHour c
              then (True, dateNow & (_localDay . gregorian . _ymdDay) %~ (+ 1))
              else (False, dateNow)
     karen <- mapM (uncurry (getKaren date)) (rssFeeds c)
     einstein <- getEinstein date
     let rest =
           catMaybes (einstein : karen)
     return (View rest (if tomorrow
                           then "Imorgon"
                           else "Idag"))
