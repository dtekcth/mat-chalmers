{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE LambdaCase #-}
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
import           Data.IORef                               ( IORef
                                                          , newIORef
                                                          , writeIORef
                                                          )
import           Data.Foldable                            ( for_ )
import           Data.Functor                             ( (<&>) )
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
import           Data.Text.Lazy                           ( pack )
import           Lens.Micro.Platform                      ( (^.)
                                                          , (&)
                                                          , (%~)
                                                          , view
                                                          )
import           System.Directory                         ( listDirectory
                                                          , getAccessTime
                                                          , removeFile )
import           Text.Printf                              ( printf )
import           Network.Wreq                             ( get
                                                          , responseBody )

import           Config
import           Model.Types
import           Model.Karen
import           Model.Wijkanders
import           Model.Linsen
import           Util                                     ( (^.^) )

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

-- | Deletes logs in the logs folder that are older than `_cLogAge`
removeOldLogs :: ( MonadIO m
                 , MonadLog (WithTimestamp (Doc ann)) m
                 , MonadReader Config m
                 ) => m ()
removeOldLogs = do
  now <- liftIO getCurrentTime
  offset <- asks _cLogAge
  path <- asks _cLogPath
  liftIO (listDirectory path) >>=
    mapM (\s -> liftIO (getAccessTime s) <&> (s,)) . (((path ++ "/") ++) <$>) >>=
    filterM (pure . (<= (now & _utctDay %~ (.-^ offset))) . toThyme . snd) <&>
    (fst <$>) >>= \case
      [] -> pure ()
      files -> timestamp ("Removing the following files:" <+> prettyList files) >>=
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
  rest <- sequence
    [ karenR "K\229rrestaurangen"
             "karrestaurangen"
             "21f31565-5c2b-4b47-d2a1-08d558129279"
    , karenR "S.M.A.K." "smak" "3ac68e11-bcee-425e-d2a8-08d558129279"
    , karenR "L's Kitchen" "ls-kitchen" "c74da2cf-aa1a-4d3a-9ba6-08d5569587a1"
    , liftIO (get wijkandersAPIURL) >>= (^.^ responseBody) <&>
      Restaurant "Wijkanders" (pack wijkandersAPIURL) . getWijkanders day'
    , fetchAndCreateLinsen day'
    ]

  for_ rest $ \r -> case menu r of
    Left e@(NMParseError _ _) ->
      asks _cLogPath >>= \path ->
      liftIO getCurrentTime >>=
      liftIO . flip writeFile (show e) . flip (printf "%s/%s%s.txt" path) (name r) . show
    _ -> pure ()

  return (View rest textday d)
 where
  wijkandersAPIURL = "https://www.wijkanders.se/restaurangen"
