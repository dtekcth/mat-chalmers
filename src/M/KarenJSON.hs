{-# LANGUAGE OverloadedStrings, FlexibleInstances #-}

-- |

module M.KarenJSON where

import Data.Aeson.Types
import qualified Data.HashMap.Strict as HM
import qualified Data.Vector as V
import qualified Data.Text.Lazy as TL

import M.Internal

type RestaurantGen = TL.Text -> TL.Text -> Restaurant

mkRestaurant :: [Menu] -> RestaurantGen
mkRestaurant menus name url = Restaurant name url menus

parseRestaurant :: Value -> Parser RestaurantGen
parseRestaurant = withObject "restaurant" $ \obj ->
  mkRestaurant <$> obj .: "recipeCategories"

parseMenus :: Value -> Parser [Menu]
parseMenus = withArray "array of menus" $ \arr ->
  mapM parseMenu (V.toList arr)

parseMenu :: Value -> Parser Menu
parseMenu = withObject "menu" $ \obj ->
  Menu <$> obj .: "name"
       <*> case HM.lookup "recipes" obj of
             Just recipes -> withArray "array of recipes" recipe recipes
             _ -> typeMismatch "recipes" (Object obj)

  where recipe :: Array -> Parser TL.Text
        recipe arr = withObject' "recipe" (V.head arr) $ \obj ->
          case HM.lookup "displayNames" obj of
            Just displayNames -> withArray "array of display names" displayName displayNames
            _                 -> typeMismatch "array of display names" (Array arr)

        withObject' :: String -> Value -> (Object -> Parser a) -> Parser a
        withObject' expected val parser = withObject expected parser val

        displayName :: Array -> Parser TL.Text
        displayName arr = withObject "display name" (.: "displayName") (V.head arr)

instance FromJSON Menu
  where parseJSON = parseMenu

instance FromJSON RestaurantGen
  where parseJSON = parseRestaurant
