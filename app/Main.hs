{-# LANGUAGE BangPatterns, LambdaCase, OverloadedStrings, TemplateHaskell #-}

module Main
  ( main
  )
where

import           Control.Concurrent                       ( MVar
                                                          , forkIO
                                                          , newMVar
                                                          , threadDelay
                                                          , tryPutMVar
                                                          )
import           Control.Monad                            ( forever )
import           Control.Monad.Reader                     ( runReaderT )
import           Control.Monad.Trans                      ( liftIO )
import           Data.FileEmbed                           ( embedDir )
import           Data.IORef                               ( IORef
                                                          , readIORef
                                                          )
import           Lens.Micro.Platform                      ( (<&>)
                                                          , set
                                                          , view
                                                          )
import           Network.Wai.Middleware.RequestLogger     ( logStdout )
import           Network.Wai.Middleware.StaticEmbedded    ( static )
import           System.Console.GetOpt                    ( ArgDescr(..)
                                                          , ArgOrder(..)
                                                          , OptDescr(..)
                                                          , getOpt
                                                          , usageInfo
                                                          )
import           System.Environment                       ( getArgs )
import           Web.Scotty                               ( get
                                                          , html
                                                          , middleware
                                                          , redirect
                                                          , scotty
                                                          )

import           Config
import           Model
import           Model.Types                              ( ClientContext(..)
                                                          , runClientT
                                                          )
import           View                                     ( render )

opts :: [OptDescr (Config -> Config)]
opts =
  [ Option [] ["help"] (NoArg (set cHelp True))           "Show usage info"
  , Option [] ["port"] (ReqArg (set cPort . read) "PORT") "Port to run on"
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
                (viewRef, refreshAction) <- runReaderT (runClientT refresh)
                                                       (ClientContext config)
                -- updater thread
                forkIO . forever $ runReaderT (runClientT (refreshAction upd))
                                              (ClientContext config)
                -- timer thread
                forkIO . forever $ tryPutMVar upd () >> threadDelay
                  (view cInterval config)
                -- Web server thread
                serve config viewRef upd
  where usage = putStrLn $ usageInfo "mat-chalmers [OPTION...]" opts

serve
  :: Config
  -> IORef View -- ^ View model
  -> MVar () -- ^ Update signal
  -> IO ()
serve conf viewRef upd = scotty (view cPort conf) $ do
  middleware logStdout
  middleware (static $(embedDir "static"))
  get "/"  site
  get "/r" (liftIO (tryPutMVar upd ()) >> redirect "/") -- force update
  where site = (html . render) =<< liftIO (readIORef viewRef)
