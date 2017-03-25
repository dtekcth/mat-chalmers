-- |

module Util where

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
