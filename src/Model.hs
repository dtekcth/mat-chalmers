{-# LANGUAGE FlexibleContexts, OverloadedStrings #-}
module Model
  ( refresh
  , Restaurant(..)
  , Menu(..)
  , View(..)
  )
where

import           Control.Arrow                            ( (>>>) )
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
import           Data.Functor                             ( (<&>) )
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
refresh
  = liftIO
      (getZonedTime >>= (view _zonedTimeToLocalTime >>> newIORef . View [] ""))
    <&> \ref ->
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
  let (textday, d) =
        if (dateNow ^. (_localTimeOfDay . _todHour)) >= view cNextDayHour c
          then
            ("Tomorrow", dateNow & (_localDay . gregorian . _ymdDay) %~ (+ 1))
          else ("Today", dateNow)
  let day'    = d ^. _localDay
  let weekday = (d ^. (_localDay . mondayWeek . _mwDay)) - 1
  let karenR  = fetchAndCreateRestaurant day'
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
    , karenR "L's Kitchen" "ls-kitchen" "c74da2cf-aa1a-4d3a-9ba6-08d5569587a1"
    , fmap
      (Restaurant "Wijkanders" (pack wijkanders) . (>>= getWijkanders day'))
      (safeGetBS wijkanders)
    ]

  for_ rest $ \r -> case menu r of
    Left e ->
      logMessage =<< timestamp (pretty $ name r <> ": " <> pack (show e))
    _ -> pure ()

  return (View rest textday d)
 where
    -- Restaurant api links
  linsenToday
    = "http://carboncloudrestaurantapi.azurewebsites.net/api/menuscreen/getdataday?restaurantid=33"
  einstein         = "http://restaurang-einstein.se/"
  wijkanders       = "http://www.wijkanders.se/restaurangen/"
  johannebergLunch = "https://chalmerskonferens.se/lunchmenyer-johanneberg/"
