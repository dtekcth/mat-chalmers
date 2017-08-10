{-# LANGUAGE OverloadedStrings #-}
-- |
module M.Karen where

import qualified Data.Text.Lazy as T
import           Data.Aeson (decode)
import           Data.Aeson.Types (Parser(..), fromJSON, parseEither)

import           M.Internal hiding (menu)
import           M.KarenJSON
import           Util

-- | Get a restaurant that kåren has.
getKaren :: Int -> T.Text -> String -> T.Text -> IO (Maybe Restaurant)
getKaren weekday name restUrl menuUrl = do
  Just text <- handle' (get' restUrl)
  case decode text of
    Just val -> case parseEither parseRestaurants val of
      Right rests -> return $ safeIndex weekday rests
                           <*> pure name
                           <*> pure menuUrl
      Left msg    -> fail msg

    Nothing  -> fail "could not decode JSON"

  where safeIndex :: (Monad m, Num b, Eq b) => b -> [a] -> m a
        safeIndex 0 (x:_) = pure x
        safeIndex n (_:xs) = safeIndex (n - 1) xs
        safeIndex _ _      = fail "safeIndex: index out of bounds"

getDayName :: Int -> T.Text
getDayName d | d >= 0 && d < 7 =
               [ "Måndag"
               , "Tisdag"
               , "Onsdag"
               , "Torsdag"
               , "Fredag"
               , "Lördag"
               , "Söndag" ] !! d
