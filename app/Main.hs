{-# LANGUAGE BangPatterns, LambdaCase, OverloadedStrings, TemplateHaskell #-}

module Main
  ( main
  )
where

import           Control.Concurrent                       ( MVar
                                                          , newMVar
                                                          , threadDelay
                                                          , tryPutMVar
                                                          )
import qualified Control.Concurrent.Async                 as Async
import           Control.Monad                            ( forever )
import           Control.Monad.Log                        ( defaultBatchingOptions
                                                          , renderWithTimestamp
                                                          , runLoggingT
                                                          , withFDHandler
                                                          )
import           Control.Monad.Reader                     ( runReaderT )
import           Control.Monad.Trans                      ( liftIO )
import           Data.FileEmbed                           ( embedDir )
import           Data.IORef                               ( IORef
                                                          , readIORef
                                                          )
import           Data.Time.Format                         ( defaultTimeLocale
                                                          , formatTime
                                                          )
import           Lens.Micro.Platform                      ( (<&>)
                                                          , set
                                                          , view
                                                          )
import           Network.HTTP.Client.TLS                  ( newTlsManager )
import           Network.Wai.Handler.Warp                 ( run )
import           Network.Wai.Middleware.RequestLogger     ( logStdout )
import           Network.Wai.Middleware.StaticEmbedded    ( static )
import           System.Console.GetOpt                    ( ArgDescr(..)
                                                          , ArgOrder(..)
                                                          , OptDescr(..)
                                                          , getOpt
                                                          , usageInfo
                                                          )
import           System.Environment                       ( getArgs )
import           System.IO                                ( stdout )
import           Data.Text.Lazy.Encoding                  ( encodeUtf8 )
import           Web.Twain

import           Config
import           Model
import           Model.Types                              ( ClientContext(..) )
import           View                                     ( render )

opts :: [OptDescr (Config -> Config)]
opts =
  [ Option [] ["help"]    (NoArg (set cHelp True))           "Show usage info"
  , Option [] ["port"]    (ReqArg (set cPort . read) "PORT") "Port to run on"
  , Option []
           ["interval"]
           (ReqArg (set cInterval . (1000000 *) . read) "INTERVAL (s)")
           "Update interval"
  ]

main :: IO ()
main =
  getArgs
    <&> getOpt Permute opts
    >>= \case
          (_     , _    , _ : _) -> usage
          (_     , _ : _, _    ) -> usage
          (!confs, _    , _    ) -> do
            let config = foldl (flip id) defaultConfig confs
            if view cHelp config
              then usage
              else do
                upd                      <- newMVar () -- putMVar when to update
                mgr                      <- newTlsManager
                (viewRef, refreshAction) <- runLoggingT
                  (runReaderT refresh (ClientContext config mgr))
                  print

                Async.concurrently_
                  (Async.concurrently_
                    -- timer
                    (forever $ tryPutMVar upd () >> threadDelay (view cInterval config))
                    -- webserver
                    (webserver config viewRef upd))
                  -- updater
                  (forever
                    $ withFDHandler defaultBatchingOptions stdout 1.0 80
                    $ \logCallback ->
                        runReaderT (refreshAction upd) (ClientContext config mgr)
                          `runLoggingT` ( logCallback
                                        . renderWithTimestamp
                                            (formatTime
                                              defaultTimeLocale
                                              "T%H:%M:%S"
                                            )
                                            id
                                        ))
  where usage = putStrLn $ usageInfo "mat-chalmers [OPTION...]" opts

webserver
  :: Config
  -> IORef View -- ^ View model
  -> MVar () -- ^ Update signal
  -> IO ()
webserver Config{_cPort=webserverPort} viewRef upd =
  run webserverPort $ foldr ($) (notFound missing)
    [ middleware . get "/"  index
    , middleware . get "/r" forceUpdate
    ]
 where
   index :: ResponderM a
   index = do
     theCurrentView <- liftIO (readIORef viewRef)
     (send . html . encodeUtf8 . render) theCurrentView

   forceUpdate :: ResponderM a
   forceUpdate = do
     _ <- liftIO $ tryPutMVar upd ()
     send   $ redirect302 "/"

   missing :: ResponderM a
   missing = send $ html "Not found..."

   middleware = logStdout . static $(embedDir "static")

