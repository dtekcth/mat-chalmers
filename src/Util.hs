module Util where

import           Control.Exception
import           Data.ByteString.Lazy                     ( ByteString )
import           Data.Text.Lazy                           ( Text )
import           Data.Text.Lazy.Encoding                  ( decodeUtf8 )
import           Network.HTTP.Conduit

takeNext :: [a] -> [a]
takeNext = take 1 . drop 1

safeGet :: String -> IO (Maybe Text)
safeGet = handle' . get

safeGetBS :: String -> IO (Maybe ByteString)
safeGetBS = handle' . getBS

get :: String -> IO Text
get = fmap decodeUtf8 . getBS

getBS :: String -> IO ByteString
getBS = simpleHttp

-- | Handler for HttpExceptions
handle' :: IO a -> IO (Maybe a)
handle' a = handle handler (fmap Just a)
 where
  handler :: HttpException -> IO (Maybe a)
  handler _ = return Nothing
