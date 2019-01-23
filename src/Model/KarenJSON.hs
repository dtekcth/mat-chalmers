{-# LANGUAGE FlexibleInstances, OverloadedStrings #-}

-- | Module for parsing JSON data from the Student Union Restaurant
-- api @ http://carboncloudrestaurantapi.azurewebsites.net/api/menuscreen

module Model.KarenJSON where

import           Control.Arrow                            ( (>>>) )
import           Data.Aeson.Types
import           Data.Maybe                               ( maybeToList )
import           Data.Text                                ( unpack )
import           Data.Thyme
import           Data.Traversable                         ( traverse
                                                          , for
                                                          )
import           Safe                                     ( headMay )
import           System.Locale                            ( defaultTimeLocale )


import           Model.Types                              ( Menu(..) )


-- | Get the menu for a specific day.
-- Nothing if there is no menu for that day.
parseMenuForDay :: Day -> Value -> Parser (Maybe Menu)
parseMenuForDay day = withObject "top" $ \obj -> fmap
  (((headMay . concat) =<<) . lookup day)
  (traverse parseMenus =<< obj .: "menus")

parseMenus :: Value -> Parser (Day, [[Menu]])
parseMenus = withObject "days" $ \menu ->
  (,)
    <$> (menu .: "menuDate" >>= withText
          "Local time"
          (unpack >>> parseTime defaultTimeLocale "%FT%T" >>> maybe
            (fail "Could not parse ISO 8601 datetime!")
            (pure . localDay)
          )
        )
    <*> (traverse parseMenus' . maybeToList =<< menu .:? "recipeCategories")

parseMenus' :: Value -> Parser [Menu]
parseMenus' = withObject "day" $ \obj -> do
  name <- obj .: "name"
  recs <- obj .: "recipes"
  for recs $ withObject "recipe" $ \rec' -> do
    [Object sv, Object _] <- rec' .: "displayNames"
    what'                 <- sv .: "displayName"
    return $ Menu name what'
