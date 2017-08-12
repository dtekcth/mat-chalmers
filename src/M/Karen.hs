{-# LANGUAGE OverloadedStrings #-}
-- |
module M.Karen where

import           Data.Aeson (decode)
import           Data.Aeson.Types (parseEither)
import qualified Data.Text.Lazy as T

import           M.Internal hiding (menu, name, url, day)
import           M.KarenJSON (RestaurantGen, parseRestaurants)
import           Util (safeIdx)

-- | Get a restaurant that kåren has.
getKaren :: Int -> T.Text -> String -> T.Text -> IO (Maybe Restaurant)
getKaren weekday name restUrl menuUrl = do
  Just text <- handle' (get' restUrl)
  case decode text of
    Just val -> case parseEither parseRestaurants val of
      Right rests -> return . Just $ getRestaurant rests name menuUrl weekday
      Left  msg   -> fail msg

    Nothing  -> fail "could not decode JSON"

getRestaurant :: [RestaurantGen] -> T.Text -> T.Text -> Int -> Restaurant
getRestaurant rests name url day =
  noLunchHandler $ (\f -> f name url) <$> safeIdx rests day

  where noLunchHandler :: Maybe Restaurant -> Restaurant
        noLunchHandler (Just r) = r
        noLunchHandler Nothing  = Restaurant name url []
