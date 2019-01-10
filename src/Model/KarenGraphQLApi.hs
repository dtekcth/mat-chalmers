{-# LANGUAGE DeriveAnyClass, DeriveGeneric, FlexibleContexts, OverloadedStrings, QuasiQuotes #-}

module Model.KarenGraphQLApi
  ( fetchAndCreateRestaurant
  )
where

import           Control.Error.Util                       ( note )
import           Control.Monad.Catch                      ( MonadThrow )
import           Control.Monad.IO.Class                   ( MonadIO
                                                          , liftIO
                                                          )
import           Control.Monad.Reader                     ( MonadReader
                                                          , asks
                                                          )
import           Data.Aeson                               ( object
                                                          , (.=)
                                                          , encode
                                                          , FromJSON(parseJSON)
                                                          , ToJSON
                                                          , (.:)
                                                          , withObject
                                                          , Object
                                                          , eitherDecode
                                                          , Value
                                                          )
import           Data.Aeson.Types                         ( Parser
                                                          , parseEither
                                                          )
import           Data.Bifunctor                           ( first )
import           Data.ByteString.Lazy                     ( ByteString )
import qualified Data.ByteString.Lazy.Char8    as BL8
import           Data.List                                ( find )
import           Data.Maybe                               ( mapMaybe )
import           Data.Text.Lazy                           ( Text
                                                          , pack
                                                          , unpack
                                                          )
import           GHC.Generics                             ( Generic )
import           Network.HTTP.Client                      ( RequestBody(..)
                                                          , method
                                                          , parseRequest
                                                          , requestBody
                                                          , requestHeaders
                                                          , responseBody
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

data Language
  = Swedish
  | English
  deriving (FromJSON, Generic, Show, Eq)

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
    , unit    :: Text
    , variant :: Text
    }
  deriving (Show)

deepLookup [prop        ] obj = obj .: prop
deepLookup (prop : props) obj = obj .: prop >>= deepLookup props

instance FromJSON Meal where
  parseJSON =
    withObject "Meal Descriptor Object" $ \obj ->
      Meal
        <$> obj .: "displayNames"
        <*> deepLookup ["mealProvidingUnit", "mealProvidingUnitName"] obj
        <*> deepLookup ["dishType", "name"] obj

parseResponse :: Value -> Parser [Meal]
parseResponse =
  withObject "ag" (deepLookup ["data", "dishOccurrencesByTimeRange"])

nameOf :: Language -> Meal -> Maybe Text
nameOf lang = fmap name . find ((== lang) . language) . names

-- | Fetches menus from Kåren's GraphQL API.
-- Parameters: Language, RestaurantUUID, day
fetchMenu
  :: (Applicative m, MonadIO m, MonadReader ClientContext m, MonadThrow m)
  => Language
  -> String
  -> String
  -> m (Either NoMenu [Menu])
fetchMenu lang restaurantUUID day = do
  initialRequest <- parseRequest apiURL
  response       <- safeBS
    (initialRequest
      { method         = "POST"
      , requestBody    = RequestBodyLBS
                           (encode $ requestData restaurantUUID day day)
      , requestHeaders = [("Content-Type", "application/json")]
      }
    )
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
    <$> fetchMenu Swedish (unpack uuid) theDate

