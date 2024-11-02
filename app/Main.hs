{-# LANGUAGE BangPatterns, LambdaCase, OverloadedStrings, TemplateHaskell #-}

module Main
  ( main
  ) where

import           Control.Concurrent                       ( MVar
                                                          , newEmptyMVar
                                                          , threadDelay
                                                          , tryPutMVar
                                                          )
import qualified Control.Concurrent.Async      as Async
import           Control.Monad                            ( forever )
import           Effectful                                ( runEff )
import           Effectful.Reader.Dynamic                 ( runReader )
import           Effectful.Log                            ( runLog, defaultLogLevel )
import           Log.Backend.StandardOutput               ( withStdOutLogger )
import           Control.Monad.Trans                      ( liftIO )
import           Data.FileEmbed                           ( embedDir )
import           Data.Foldable                            ( traverse_ )
import           Data.IORef                               ( IORef
                                                          , readIORef
                                                          )
import           Lens.Micro.Platform                      ( set
                                                          , view
                                                          )
import           Network.Wai.Handler.Warp                 ( run )
import           Network.Wai.Middleware.RequestLogger     ( logStdout )
import           Network.Wai.Middleware.StaticEmbedded    ( static )
import           System.Console.GetOpt                    ( ArgDescr(..)
                                                          , ArgOrder(..)
                                                          , OptDescr(..)
                                                          , getOpt
                                                          , usageInfo
                                                          )
import           System.Directory                         ( createDirectoryIfMissing )
import           System.Environment                       ( getArgs )
import           Data.Text.Lazy.Encoding                  ( encodeUtf8 )
import           Web.Twain                                ( get
                                                          , html
                                                          , notFound
                                                          , redirect302
                                                          , send
                                                          )

import           Config
import           Model
import           View                                     ( render )
import Effectful.FileSystem (runFileSystem)

opts :: [OptDescr (Config -> Config)]
opts =
  [ Option [] ["help"] (NoArg  (set cHelp True))           "Show usage info"
  , Option [] ["port"] (ReqArg (set cPort . read) "PORT")  "Port to run on"
  , Option [] ["path"] (ReqArg (set cLogPath) "PATH")      "Path to save log files to, default is 'logs'"
  , Option [] ["age"]  (ReqArg (set cLogAge . read) "AGE") "Amount of days to keep logs"
  , Option []
           ["interval"]
           (ReqArg (set cInterval . (1000000 *) . read) "INTERVAL (s)")
           "Update interval"
  ]

usage :: IO ()
usage = putStrLn $ usageInfo "mat-chalmers [OPTION...]" opts

main :: IO ()
main = (recreateConfig . getOpt Permute opts <$> getArgs) >>= \case
  (_                       , _    , _ : _) -> usage
  (_                       , _ : _, _    ) -> usage
  (Config { _cHelp = True }, _    , _    ) -> usage
  (config                  , _    , _    ) -> do
    upd     <- newEmptyMVar -- putMVar when to update
    viewRef <- createViewReference
    createDirectoryIfMissing True (_cLogPath config)

    -- In the list there are three items running concurrently:
    -- 1. Timer that sends a signal to the updater when it's time to update
    -- 2. Webserver that serves the menus to the user
    -- 3. Updater that fetches new data from the restaurants
    Async.runConcurrently $ traverse_
      Async.Concurrently
      [ timer upd config
      , webserver config viewRef upd
      , updater upd viewRef config
      ]
 where
  timer upd cfg =
    forever $ tryPutMVar upd () >> threadDelay (view cInterval cfg)

  updater upd viewRef cfg =
    forever .
    runEff .
    runReader cfg .
    runFileSystem .
    withStdOutLogger $ \logger ->
                         runLog "main" logger defaultLogLevel
                         (refresh viewRef upd)

webserver
  :: Config
  -> IORef View -- ^ View model
  -> MVar () -- ^ Update signal
  -> IO ()
webserver Config{_cPort=webserverPort} viewRef upd =
  run webserverPort $ foldr
    (logStdout . static $(embedDir "static") <$>)
    (notFound (send $ html "not found..."))
    [ get "/"  (liftIO (readIORef viewRef) >>= send . html . encodeUtf8 . render)
    , get "/r" (liftIO (tryPutMVar upd ()) >> send (redirect302 "/"))
    ]
