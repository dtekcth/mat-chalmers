{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE BangPatterns #-}
module Main (main) where

import           Control.Concurrent
import           Control.Lens
import           Control.Monad
import           Control.Monad.Trans (liftIO)
import           Data.FileEmbed
import           Data.IORef
import           Network.Wai.Middleware.RequestLogger
import           Network.Wai.Middleware.StaticEmbedded
import           System.Console.GetOpt
import           System.Environment
import           Web.Scotty hiding (options)

import           Config
import           Model
import           View (render)

opts :: [OptDescr (Config -> Config)]
opts =
  [ Option [] ["help"] (NoArg (set cHelp True)) "Show usage info"
  , Option [] ["port"] (ReqArg (set cPort . read) "PORT") "Port to run on"
  , Option
      []
      ["interval"]
      (ReqArg (set cInterval . (1000000 *) . read) "INTERVAL (s)")
      "Update interval"
  ]

main :: IO ()
main = do
  args <- getArgs
  case getOpt Permute opts args of
    (_, _, _:_) -> usage
    (_, _:_, _) -> usage
    (!confs, _, _) -> do
      let config = foldl (flip id) defaultConfig confs

      if view cHelp config
        then usage
        else do
        upd <- newMVar () -- putMVar when to update
        (viewRef, refreshAction) <- refresh config
        -- updater thread
        forkIO . forever $ refreshAction upd
        -- timer thread
        forkIO . forever $ tryPutMVar upd () >> threadDelay (view cInterval config)
        -- serve webpage
        serve config viewRef upd
  where
    usage = putStrLn $ usageInfo "mat-chalmers [OPTION...]" opts

serve
  :: Config
  -> IORef View -- View model
  -> MVar () -- Update signal
  -> IO ()
serve conf viewRef upd =
  scotty (view cPort conf) $ do
    middleware logStdout
    middleware (static $(embedDir "static"))
    get "/" site
    get "/r" (liftIO (tryPutMVar upd ()) >> redirect "/") -- force update
  where
    site = (html . render) =<< liftIO (readIORef viewRef)
