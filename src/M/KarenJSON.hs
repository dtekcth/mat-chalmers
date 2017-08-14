{-# LANGUAGE OverloadedStrings, FlexibleInstances #-}

-- | Module for parsing JSON data from the Student Union Restaurant
-- api @ http://carboncloudrestaurantapi.azurewebsites.net/api/menuscreen

module M.KarenJSON where

import Data.Aeson.Types
import qualified Data.Text as TS
import Data.Thyme
import System.Locale (defaultTimeLocale)
import Data.Traversable (traverse)

import M.Types hiding (name, menu, url)

-- | Get the menu for a specific day. Nothing if there is no menu for
-- that day.
parseMenuForDay :: Day -> Value -> Parser (Maybe [Menu])
parseMenuForDay day = withObject "top" $ \obj -> do
  menuArray <- obj .: "menus"
  menus <- traverse parseMenus menuArray
  return $ lookup day menus

parseMenus :: Value -> Parser (Day, [Menu])
parseMenus = withObject "days" $ \menu -> do
  day <- fmap localDay $ menu .: "menuDate"
  recips <- menu .: "recipeCategories"
  menu' <- traverse parseMenu recips
  return (day, menu')

parseMenu :: Value -> Parser Menu
parseMenu = withObject "day" $ \obj -> do
  name <- obj .: "name"
  (Object recip1:_) <- obj .: "recipes"
  (Object what:_) <- recip1 .: "displayNames"
  what' <- what .: "displayName"
  return $ Menu name what'

instance FromJSON LocalTime where
  parseJSON = withText "local time" $ \str ->
    case parseTime defaultTimeLocale "%FT%T" (TS.unpack str) of
      Just time -> pure time
      _         -> fail "could not parse ISO 8601 datetime"
