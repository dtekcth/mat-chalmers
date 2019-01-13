{-# LANGUAGE FlexibleContexts, OverloadedStrings, QuasiQuotes #-}

module Model.KarenGraphQLApi
  ( fetch
  , fetchMenu
  , fetchAndCreateRestaurant
  )
where

import           Control.Monad.Catch                      ( MonadThrow )
import           Control.Monad.IO.Class                   ( MonadIO )
import           Control.Monad.Reader                     ( MonadReader )
import           Data.Aeson                               ( Object
                                                          , object
                                                          , (.=)
                                                          , encode
                                                          , FromJSON(parseJSON)
                                                          , ToJSON
                                                          , (.:)
                                                          , withObject
                                                          , eitherDecode
                                                          , Value
                                                          )
import           Data.Aeson.Types                         ( Parser
                                                          , parseEither
                                                          )
import           Data.Bifunctor                           ( first )
import qualified Data.ByteString.Lazy.Char8    as BL8
import           Data.List                                ( find )
import           Data.Maybe                               ( mapMaybe )
import qualified Data.Text                     as T
import           Data.Text.Lazy                           ( Text
                                                          , unpack
                                                          )
import           Network.HTTP.Client                      ( RequestBody(..)
                                                          , method
                                                          , parseRequest
                                                          , requestBody
                                                          , requestHeaders
                                                          )
import           Text.Heredoc                             ( str )

import           Model.Types                              ( ClientContext(..)
                                                          , NoMenu(..)
                                                          , Menu(..)
                                                          , Restaurant
                                                            ( Restaurant
                                                            )
                                                          )
import           Util                                     ( menusToEitherNoLunch
                                                          , safeBS
                                                          )

apiURL :: String
apiURL = "https://heimdallprod.azurewebsites.net/graphql"

-- brittany-disable-next-binding
graphQLQuery :: String
graphQLQuery
  = [str|query DishOccurrencesByTimeRangeQuery($mealProvidingUnitID: String, $startDate: String, $endDate: String) {
        |  dishOccurrencesByTimeRange(mealProvidingUnitID: $mealProvidingUnitID, startDate: $startDate, endDate: $endDate) {
        |    ...MenuDishOccurrence
        |  }
        |}
        |
        |fragment MenuDishOccurrence on DishOccurrence {
        |  displayNames {
        |    name
        |    categoryName
        |  }
        |  startDate
        |  dishType {
        |    name
        |  }
        |  dish {
        |    ...MenuDish
        |  }
        |  mealProvidingUnit {
        |    mealProvidingUnitName
        |    id
        |  }
        |}
        |
        |fragment MenuDish on Dish {
        |  name
        |  price
        |  recipes {
        |    portions
        |    name
        |    allergens {
        |      id
        |      name
        |      imageUrl
        |      sortOrder
        |    }
        |  }
        |}|]

type Language = String

data MealName =
  MealName
    { name     :: Text
    , language :: Language
    }
  deriving (Show)

instance FromJSON MealName where
  parseJSON =
    withObject "MealName" $ \obj ->
      MealName
        <$> obj .: "name"
        <*> obj .: "categoryName"

data Meal =
  Meal
    { names   :: [MealName]
    , variant :: Text
    }
  deriving (Show)

instance FromJSON Meal where
  parseJSON =
    withObject "Meal Descriptor Object" $ \obj ->
      Meal
        <$> obj .: "displayNames"
        <*> (obj .: "dishType" >>= (.: "name"))

parseResponse :: Value -> Parser [Meal]
parseResponse = withObject "Parse meals"
  $ \obj -> obj .: "data" >>= (.: "dishOccurrencesByTimeRange")

nameOf :: Language -> Meal -> Maybe Text
nameOf lang = fmap name . find ((== lang) . language) . names

fetch
  :: (MonadIO m, MonadReader ClientContext m, MonadThrow m)
  => String
  -> String
  -> m (Either NoMenu BL8.ByteString)
fetch restaurantUUID day = do
  initialRequest <- parseRequest apiURL
  safeBS
    (initialRequest
      { method         = "POST"
      , requestBody    = RequestBodyLBS
                           (encode $ requestData restaurantUUID day day)
      , requestHeaders = [("Content-Type", "application/json")]
      }
    )
 where
  requestData :: (ToJSON a, ToJSON b) => a -> b -> b -> Value
  requestData unitID startDate endDate = object
    [ "query" .= graphQLQuery
    , "operationName" .= ("DishOccurrencesByTimeRangeQuery" :: String)
    , "variables" .= object
      [ "mealProvidingUnitID" .= unitID
      , "startDate" .= startDate
      , "endDate" .= endDate
      ]
    ]

-- | Fetches menus from Kåren's GraphQL API.
-- Parameters: Language, RestaurantUUID, day
fetchMenu
  :: (MonadIO m, MonadReader ClientContext m, MonadThrow m)
  => Language
  -> String
  -> String
  -> m (Either NoMenu [Menu])
fetchMenu lang restaurantUUID day = do
  response <- fetch restaurantUUID day
  pure
    $   response
    >>= failWithNoMenu eitherDecode
    >>= failWithNoMenu (parseEither parseResponse)
    >>= menusToEitherNoLunch
    .   mapMaybe (\m -> Menu (variant m) <$> nameOf lang m)
 where
  failWithNoMenu :: Show a => (a -> Either String b) -> a -> Either NoMenu b
  failWithNoMenu action x =
    first (\msg -> NMParseError msg . BL8.pack . show $ x) (action x)

-- | Parameters: the date to fetch, title, tag, uuid
fetchAndCreateRestaurant
  :: (MonadIO m, MonadReader ClientContext m, MonadThrow m)
  => String
  -> Text
  -> Text
  -> Text
  -> m Restaurant
fetchAndCreateRestaurant theDate title tag uuid =
  Restaurant
      title
      (  "http://carbonatescreen.azurewebsites.net/menu/week/"
      <> tag
      <> "/"
      <> uuid
      )
    <$> fetchMenu "Swedish" (unpack uuid) theDate
