{-# LANGUAGE FlexibleContexts, NumericUnderscores, OverloadedStrings #-}
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
import           Control.Monad                            ( filterM )
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
import           Control.Retry                            ( fibonacciBackoff
                                                          , limitRetries
                                                          )
import           Data.IORef                               ( IORef
                                                          , newIORef
                                                          , writeIORef
                                                          )
import           Data.Foldable                            ( for_ )
import           Data.Functor                             ( (<&>) )
import           Data.Text.Lazy                           ( fromStrict )
import           Prettyprinter                            ( Doc
                                                          , prettyList
                                                          , (<+>)
                                                          )
import           Data.AffineSpace                         ( (.+^)
                                                          , (.-^)
                                                          )
import           Data.Thyme                               ( _localDay
                                                          , _localTimeOfDay
                                                          , _todHour
                                                          , _zonedTimeToLocalTime
                                                          , getZonedTime
                                                          , getCurrentTime
                                                          , _utctDay
                                                          )
import           Data.Thyme.Time                          ( toThyme )
import           Lens.Micro.Platform                      ( (^.)
                                                          , (&)
                                                          , (%~)
                                                          , view
                                                          )
import           System.Directory                         ( listDirectory
                                                          , getAccessTime
                                                          , removeFile )
import           Text.Printf                              ( printf )
import           Network.HTTP.Req

import           Config
import           Model.Types
import           Model.Karen
import           Model.Wijkanders
import           Model.Linsen

-- | Refreshes menus.
-- The refresh function evaluates to `Some monad m => m (View model, Update signal)`,
-- where the View model has all the current data. Call update signal to get
-- new data from the data sources.
refresh
  :: ( Monad m
     , MonadIO m
     , MonadLog (WithTimestamp (Doc ann)) m
     , MonadReader Config m
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

-- | Deletes old logs in the logs folder, that are
removeOldLogs :: ( MonadIO m
                 , MonadLog (WithTimestamp (Doc ann)) m
                 , MonadReader Config m
                 ) => m ()
removeOldLogs =
      liftIO getCurrentTime >>= \now ->
      asks _cLogAge >>= \offset ->
      let old = now & _utctDay %~ (.-^ offset) in
      asks _cLogPath >>= \path ->
      liftIO (listDirectory path) >>=
      mapM (\s -> liftIO (getAccessTime s) <&> (s,)) . fmap ((path ++ "/") ++) >>=
      fmap (fmap fst) . filterM (pure . (<= old) . toThyme . snd) >>= \files ->
      timestamp ("Removing the following files:" <+> prettyList files) >>=
      logMessage >>
      liftIO (mapM_ removeFile files)

update
  :: ( MonadIO m
     , MonadLog (WithTimestamp (Doc ann)) m
     , MonadReader Config m
     , MonadThrow m
     )
  => m View
update = do
  nextDayHour <- asks _cNextDayHour
  dateNow     <- liftIO $ fmap (view _zonedTimeToLocalTime) getZonedTime
  let (textday, d) =
        if dateNow ^. _localTimeOfDay . _todHour >= nextDayHour
          then ("Tomorrow", dateNow & _localDay %~ (.+^ 1))
          else ("Today", dateNow)
      day'    = d ^. _localDay
      karenR  = fetchAndCreateRestaurant day'
  removeOldLogs
  rest <- runReq (
            defaultHttpConfig {
              httpConfigRetryPolicy = fibonacciBackoff 30_000_000 <> limitRetries 5
            }) $ sequence
    [ karenR "K\229rrestaurangen"
             "karrestaurangen"
             "21f31565-5c2b-4b47-d2a1-08d558129279"
    , karenR "S.M.A.K." "smak" "3ac68e11-bcee-425e-d2a8-08d558129279"
    , karenR "L's Kitchen" "ls-kitchen" "c74da2cf-aa1a-4d3a-9ba6-08d5569587a1"
    , Restaurant "Wijkanders" (fromStrict $ renderUrl wijkandersAPIURL) .
      getWijkanders day' . responseBody <$> req GET wijkandersAPIURL NoReqBody lbsResponse mempty
    , fetchAndCreateLinsen day'
    ]

  for_ rest $ \r -> case menu r of
    Left e ->
      asks _cLogPath >>= \path ->
      liftIO getCurrentTime >>=
      liftIO . flip writeFile (show e) . flip (printf "%s/%s%s.txt" path) (name r) . show
    Right _ -> pure ()

  return (View rest textday d)
 where
  wijkandersAPIURL = http "www.wijkanders.se" /: "restaurangen"
