-- |

module Util where

import Control.Exception
import Control.Monad
import Data.ByteString.Lazy (ByteString)
import Data.Text.Lazy (Text)
import Data.Text.Lazy.Encoding (decodeUtf8)
import Network.HTTP.Conduit

safeIdx :: [a] -> Int -> Maybe a
safeIdx [] _ = Nothing
safeIdx (x:xs) n
  | n == 0 = return x
  | n < 0 = Nothing
  | otherwise = safeIdx xs (n - 1)

safeHead :: [a] -> Maybe a
safeHead (x:_) = Just x
safeHead _ = Nothing

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
handle' a = handle handler (liftM Just a)
  where handler :: HttpException -> IO (Maybe a)
        handler _ = return Nothing
