{-# LANGUAGE FlexibleContexts, OverloadedStrings #-}
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
import           Control.Monad.Catch                      ( MonadThrow )
import           Control.Monad.IO.Class                   ( MonadIO
                                                          , liftIO
                                                          )
import           Control.Monad.Log                        ( MonadLog
                                                          , WithTimestamp
                                                          , logMessage
                                                          , timestamp
                                                          )
import           Control.Monad.Reader                     ( MonadReader
                                                          , asks
                                                          )
import           Data.IORef                               ( IORef
                                                          , newIORef
                                                          , writeIORef
                                                          )
import           Data.Foldable                            ( for_ )
import           Data.Text.Lazy                           ( pack )
import           Data.Text.Prettyprint.Doc                ( Doc
                                                          , pretty
                                                          )
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
import           Model.Einstein                           ( getEinstein )
import           Model.Types
import           Model.Karen
import           Model.KarenGraphQLApi
import           Model.Wijkanders
import           Util

-- | Refreshes menus.
-- The refresh function evaluates to `Some monad m => m (View model, Update signal)`,
-- where the View model has all the current data. Call update signal to get
-- new data from the data sources.
refresh
  :: ( Monad m
     , MonadIO m
     , MonadLog (WithTimestamp (Doc ann)) m
     , MonadReader ClientContext m
     , MonadThrow m
     )
  => m (IORef View, MVar () -> m ())
refresh = do
  date <- liftIO $ fmap (view _zonedTimeToLocalTime) getZonedTime
  ref  <- liftIO $ newIORef (View [] "" date)
  return
    ( ref
    , \upd -> do
      liftIO $ takeMVar upd
      logMessage =<< timestamp "Updating view..."
      v <- update
      liftIO $ writeIORef ref v
    )

update
  :: ( MonadIO m
     , MonadLog (WithTimestamp (Doc ann)) m
     , MonadReader ClientContext m
     , MonadThrow m
     )
  => m View
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
  let karenR =
        fetchAndCreateRestaurant (formatTime defaultTimeLocale "%F" date)
  rest <- sequence
    [ karenR "K\229rrestaurangen"
             "karrestaurangen"
             "21f31565-5c2b-4b47-d2a1-08d558129279"
    , karenR "Express Johanneberg"
             "johanneberg-express"
             "3d519481-1667-4cad-d2a3-08d558129279"
    , karenR "S.M.A.K." "smak" "3ac68e11-bcee-425e-d2a8-08d558129279"
    , fmap (Restaurant "Linsen" johannebergLunch . (>>= getKarenToday))
           (safeGetBS linsenToday)
    , fmap (Restaurant "Einstein" (pack einstein) . (>>= getEinstein weekday))
           (safeGetBS einstein)
    , fmap (Restaurant "L's Kitchen" lindholmenLunch . (>>= getKaren day))
           (safeGetBS ls)
    , fmap
      (Restaurant "Wijkanders" (pack wijkanders) . (>>= getWijkanders day))
      (safeGetBS wijkanders)
    ]

  for_ rest $ \r -> case menu r of
    Left e ->
      logMessage =<< timestamp (pretty $ name r <> ": " <> pack (show e))
    _ -> pure ()

  return (View rest textday date)
 where
    -- Restaurant api links
  linsenToday
    = "http://carboncloudrestaurantapi.azurewebsites.net/api/menuscreen/getdataday?restaurantid=33"
  ls
    = "http://carboncloudrestaurantapi.azurewebsites.net/api/menuscreen/getdataweek?restaurantid=8"
  einstein         = "http://restaurang-einstein.se/"
  wijkanders       = "http://www.wijkanders.se/restaurangen/"
  johannebergLunch = "https://chalmerskonferens.se/lunchmenyer-johanneberg/"
  lindholmenLunch  = "https://chalmerskonferens.se/lunchmenyer-lindholmen/"
