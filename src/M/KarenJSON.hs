{-# LANGUAGE OverloadedStrings, FlexibleInstances #-}

-- | Module for parsing JSON data from the Student Union Restaurant
-- api @ http://carboncloudrestaurantapi.azurewebsites.net/api/menuscreen

module M.KarenJSON where

import           Data.Aeson.Types
import qualified Data.HashMap.Strict as HM
import qualified Data.Vector as V
import qualified Data.Text.Lazy as TL
import qualified Data.Text as TS
import           Data.Thyme
import           System.Locale (defaultTimeLocale)
import           Data.Maybe (catMaybes)

import           M.Internal hiding (name, menu, lunch, url)

-- | Partially applied Restaurant, apply name and url to get a Restaurant
type RestaurantGen = TL.Text -> TL.Text -> Restaurant

mkRestaurant :: [Menu] -> RestaurantGen
mkRestaurant menus name url = Restaurant name url menus

parseRestaurants :: Value -> Parser [RestaurantGen]
parseRestaurants = withObject "week lunch menu" $ \obj ->
  obj .:: "menus" $ \menus ->
    withArray' "array of menus" menus $ \arr ->
      mapM parseRestaurant (V.toList arr)

parseRestaurant :: Value -> Parser RestaurantGen
parseRestaurant = withObject "menu object" $ \obj ->
  obj .:: "recipeCategories" $
     (fmap (mkRestaurant . catMaybes) . go "array of recipe types")

  where go :: String -> Value -> Parser [Maybe Menu]
        go str (Array rcpCats) = withArray' str (Array rcpCats) parseMenus
        go _   obj             = typeMismatch "array of recipe types" obj

parseMenus :: Array -> Parser [Maybe Menu]
parseMenus arr = mapM (withObject "lunch dish" parseMenu') (V.toList arr)

  where parseMenu' :: Object -> Parser (Maybe Menu)
        parseMenu' obj = obj .:: "recipes" $ \recps ->
          withArray "array of recipes" (parseMenu (lunchText obj)) recps

        fromJust :: Maybe a -> a
        fromJust (Just a) = a
        fromJust Nothing  = error "fromJust: Nothing"

        lunchText :: Object -> TL.Text
        lunchText = fromJust . parseMaybe (.: "name")

parseMenu :: TL.Text -> Array -> Parser (Maybe Menu)
parseMenu lunch arr = case V.toList arr of
  (r:_) -> withObject' "recipe" r $ \obj ->
    obj .:: "displayNames"
      $ (fmap (fmap (Menu lunch)) . withArray expected parseDispName)
  []     -> return Nothing

  where expected = "array of recipe texts"

parseDispName :: Array -> Parser (Maybe TL.Text)
parseDispName arr = case V.toList arr of
  (dn:_) -> mconcat $ withObject' <$> pure "recipe text"
                                  <*> pure dn
                                  <*> pure (.: "displayName")
  []     -> return Nothing

withArray' :: String -> Value -> (Array -> Parser a) -> Parser a
withArray' expected val parser = withArray expected parser val

withObject' :: String -> Value -> (Object -> Parser a) -> Parser a
withObject' expected val parser = withObject expected parser val

(.::) :: Object -> TS.Text -> (Value -> Parser a) -> Parser a
(.::) obj field parser = case HM.lookup field obj of
                           Just newObj -> parser newObj
                           Nothing     -> fail $ "key \""
                                               ++ show field
                                               ++ "\" was not found"

instance FromJSON RestaurantGen where
  parseJSON = parseRestaurant

instance FromJSON LocalTime where
  parseJSON = withText "local time" $ \str ->
    case parseTime defaultTimeLocale "%FT%T" (TS.unpack str) of
      Just time -> pure time
      _         -> fail "could not parse ISO 8601 datetime"
