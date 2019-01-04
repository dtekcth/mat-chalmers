{-# LANGUAGE OverloadedStrings #-}
module Model
  ( refresh
  , Restaurant(..)
  , Menu(..)
  , View(..)
  )
where

import           Control.Concurrent.MVar                  ( MVar
                                                          , takeMVar
                                                          )
import           Control.Monad.IO.Class                   ( liftIO )
import           Control.Monad.Reader                     ( asks )
import           Data.IORef                               ( IORef
                                                          , newIORef
                                                          , writeIORef
                                                          )
import           Data.Functor                             ( (<&>) )
import           Data.Thyme                               ( _localDay
                                                          , _localTimeOfDay
                                                          , _todHour
                                                          , _ymdDay
                                                          , _zonedTimeToLocalTime
                                                          , formatTime
                                                          , getZonedTime
                                                          , gregorian
                                                          )
import           Data.Thyme.Calendar.WeekDate             ( _mwDay
                                                          , mondayWeek
                                                          )
import           Lens.Micro.Platform                      ( (^.)
                                                          , (&)
                                                          , (%~)
                                                          , view
                                                          )
import           System.Locale                            ( defaultTimeLocale )

import           Config
import           Model.Types
import           Model.Karen
import           Model.KarenGraphQLApi
import           Util

-- | Refreshes menus.
-- The refresh function evaluates to `Client (View model, Update signal)`,
-- where the View model has all the current data. Call update signal to get
-- new data from the data sources.
refresh :: Client (IORef View, MVar () -> Client ())
refresh = do
  date <- liftIO $ fmap (view _zonedTimeToLocalTime) getZonedTime
  ref  <- liftIO $ newIORef (View [] "" date)
  return
    ( ref
    , \upd -> do
      liftIO $ takeMVar upd
      liftIO $ putStrLn "Upd"
      v <- update
      liftIO $ writeIORef ref v
    )

update :: Client View
update = do
  c       <- asks ccCfg
  dateNow <- liftIO $ fmap (view _zonedTimeToLocalTime) getZonedTime
  let (textday, date) =
        if (dateNow ^. (_localTimeOfDay . _todHour)) >= view cNextDayHour c
          then
            ("Tomorrow", dateNow & (_localDay . gregorian . _ymdDay) %~ (+ 1))
          else ("Today", dateNow)
  let day     = date ^. _localDay
  let weekday = (date ^. (_localDay . mondayWeek . _mwDay)) - 1
  let theDate = formatTime defaultTimeLocale "%F" date
  rest <- liftIO $ sequence
    [ fetchMenu "21f31565-5c2b-4b47-d2a1-08d558129279" theDate
      <&> ( Restaurant
              "K\229rrestaurangen"
              "http://carbonatescreen.azurewebsites.net/menu/week/karrestaurangen/21f31565-5c2b-4b47-d2a1-08d558129279"
          . transformMenu Swe
          )
    , fetchMenu "3d519481-1667-4cad-d2a3-08d558129279" theDate
      <&> ( Restaurant
              "Express Johanneberg"
              "http://carbonatescreen.azurewebsites.net/menu/week/johanneberg-express/3d519481-1667-4cad-d2a3-08d558129279"
          . transformMenu Swe
          )
    , fetchMenu "3ac68e11-bcee-425e-d2a8-08d558129279" theDate
      <&> ( Restaurant
              "S.M.A.K."
              "http://carbonatescreen.azurewebsites.net/menu/week/smak/3ac68e11-bcee-425e-d2a8-08d558129279"
          . transformMenu Swe
          )
    , getKarenToday "Linsen" johannebergLunch <$> safeGetBS linsenToday
--      There is no Einstein at the moment. We'll put it back when their web presence is back.
--      , getEinstein weekday <$> safeGet einstein
    , getKaren day "L's Kitchen" lindholmenLunch <$> safeGetBS ls
--      Wijkanders are hard to parse. Put them back when you have a parser.
--      , getWijkanders (weekday + 1) <$> safeGet wijkanders
    ]
  return (View rest textday date)
 where
    -- Restaurant api links
--    einstein = "http://butlercatering.se/einstein"
  karen
    = "http://carboncloudrestaurantapi.azurewebsites.net/api/menuscreen/getdataweek?restaurantid=5"
  linsenToday
    = "http://carboncloudrestaurantapi.azurewebsites.net/api/menuscreen/getdataday?restaurantid=33"
  xpress
    = "http://carboncloudrestaurantapi.azurewebsites.net/api/menuscreen/getdataweek?restaurantid=7"
  ls
    = "http://carboncloudrestaurantapi.azurewebsites.net/api/menuscreen/getdataweek?restaurantid=8"
  smak
    = "http://carboncloudrestaurantapi.azurewebsites.net/api/menuscreen/getdataweek?restaurantid=42"
  wijkanders       = "http://www.wijkanders.se/restaurangen/"

  -- Restaurant menu links
  johannebergLunch = "https://chalmerskonferens.se/lunchmenyer-johanneberg/"
  lindholmenLunch  = "https://chalmerskonferens.se/lunchmenyer-lindholmen/"
