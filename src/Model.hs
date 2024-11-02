{-# LANGUAGE OverloadedStrings, LambdaCase #-}
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
import           Data.IORef                               ( IORef
                                                          , newIORef
                                                          , writeIORef
                                                          )
import           Data.Foldable                            ( for_ )
import           Data.Functor                             ( (<&>) )
import           Effectful
import           Effectful.FileSystem
import           Effectful.Log
import           Effectful.Reader.Dynamic
import           Prettyprinter                            ( prettyList
                                                          , (<+>)
                                                          )
import           Data.AffineSpace                         ( (.+^)
                                                          , (.-^)
                                                          )
import           Data.Text                                ( pack )
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
import           Text.Printf                              ( printf )

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
  :: ( IOE :> es
     , Reader Config :> es
     , Log :> es
     , FileSystem :> es
     )
  => IORef View -> MVar () -> Eff es ()
refresh ref upd = do
  liftIO $ takeMVar upd
  logInfo_ "Updating view..."
  v <- update
  liftIO $ writeIORef ref v

createViewReference :: (MonadIO m) => m (IORef View)
createViewReference = liftIO $ do
  now <- getZonedTime
  newIORef (View [] "" (now ^. _zonedTimeToLocalTime))

-- | Deletes logs in the logs folder that are older than `_cLogAge`
removeOldLogs :: ( IOE :> es
                 , Reader Config :> es
                 , FileSystem :> es
                 , Log :> es
                 ) => Eff es ()
removeOldLogs = do
  now <- liftIO getCurrentTime
  offset <- asks _cLogAge
  path <- asks _cLogPath
  listDirectory path >>=
    mapM (\s -> getAccessTime s <&> (s,)) . (((path ++ "/") ++) <$>) >>=
    filterM (pure . (<= (now & _utctDay %~ (.-^ offset))) . toThyme . snd) <&>
    (fst <$>) >>= \case
      [] -> pure ()
      files -> logInfo_ (pack . show $ "Removing the following files:" <+> prettyList files) >>
        mapM_ removeFile files

update
  :: ( IOE :> es
     , Reader Config :> es
     , Log :> es
     , FileSystem :> es
     )
  => Eff es View
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
    , fetchAndCreateWijkanders day'
    , fetchAndCreateLinsen day'
    ]

  for_ rest $ \r -> case menu r of
    Left e@(NMParseError _ _) ->
      asks _cLogPath >>= \path ->
      liftIO getCurrentTime >>=
      liftIO . flip writeFile (show e) . flip (printf "%s/%s%s.txt" path) (name r) . show
    _ -> pure ()
  return (View rest textday d)
