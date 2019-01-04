module Util where

import           Control.Exception                        ( handle )
import           Control.Monad.Reader                     ( asks )
import           Control.Monad.Trans                      ( liftIO )
import           Data.ByteString.Lazy                     ( ByteString )
import           Data.Text.Lazy                           ( Text )
import           Data.Text.Lazy.Encoding                  ( decodeUtf8 )
import           Network.HTTP.Client                      ( HttpException
                                                          , Request
                                                          , httpLbs
                                                          , parseRequest
                                                          , responseBody
                                                          )

import           Model.Types                              ( Client(..)
                                                          , ClientContext(..)
                                                          )

takeNext :: [a] -> [a]
takeNext = take 1 . drop 1

safeGet :: String -> Client (Maybe Text)
safeGet = (fmap . fmap) decodeUtf8 . safeGetBS

safeBS :: Request -> Client (Maybe ByteString)
safeBS r = do
  m <- asks ccManager
  liftIO $ (fmap . fmap) responseBody (handle' (liftIO (httpLbs r m)))

safeGetBS :: String -> Client (Maybe ByteString)
safeGetBS = (=<<) safeBS . parseRequest

-- | Handler for HttpExceptions
handle' :: IO a -> IO (Maybe a)
handle' a = handle handler (fmap Just a)
 where
  handler :: HttpException -> IO (Maybe a)
  handler _ = return Nothing
