-- |

module Util where

import Control.Exception
import Control.Monad
import Network.HTTP.Conduit
import Data.Text.Lazy.Encoding (decodeUtf8)
import qualified Data.Text.Lazy as T

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

get :: String -> IO T.Text
get url = liftM decodeUtf8 (simpleHttp url)

get' url = simpleHttp url

-- | Handler for HttpExceptions
handle' :: IO a -> IO (Maybe a)
handle' a = handle handler (liftM Just a)
  where handler :: HttpException -> IO (Maybe a)
        handler _ = return Nothing
