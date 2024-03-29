{-# LANGUAGE FlexibleContexts, OverloadedStrings #-}
module Model
  ( Restaurant(..)
  , Menu(..)
  , View(..)
  , createViewReference
  , refresh
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
import           Prettyprinter                            ( Doc
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
import           Lens.Micro.Platform                      ( (^.)
                                                          , (&)
                                                          , (%~)
                                                          , view
                                                          )

import           Config
import           Model.Types
import           Model.Karen
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
  => IORef View -> MVar () -> m ()
refresh ref upd = do
  liftIO $ takeMVar upd
  logMessage =<< timestamp "Updating view..."
  v <- update
  liftIO $ writeIORef ref v

createViewReference :: (MonadIO m) => m (IORef View)
createViewReference = liftIO $ do
  now <- getZonedTime
  newIORef (View [] "" (now ^. _zonedTimeToLocalTime))

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
  let karenR  = fetchAndCreateRestaurant day'
  rest <- sequence
    [ karenR "K\229rrestaurangen"
             "karrestaurangen"
             "21f31565-5c2b-4b47-d2a1-08d558129279"
    , karenR "S.M.A.K." "smak" "3ac68e11-bcee-425e-d2a8-08d558129279"
    , karenR "L's Kitchen" "ls-kitchen" "c74da2cf-aa1a-4d3a-9ba6-08d5569587a1"
    , fmap
      (Restaurant "Wijkanders" (pack wijkandersAPIURL) . (>>= getWijkanders day'))
      (safeGetBS wijkandersAPIURL)
    ]

  for_ rest $ \r -> case menu r of
    Left e ->
      logMessage =<< timestamp (pretty $ name r <> ": " <> pack (show e))
    _ -> pure ()

  return (View rest textday d)
 where
  wijkandersAPIURL = "http://www.wijkanders.se/restaurangen/"
