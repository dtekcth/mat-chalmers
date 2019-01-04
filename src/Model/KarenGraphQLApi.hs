{-# LANGUAGE DeriveAnyClass, DeriveGeneric, LambdaCase, OverloadedStrings, QuasiQuotes #-}

module Model.KarenGraphQLApi
  ( fetchMenu
  , Language(..)
  )
where

import           Control.Error.Util                       ( note )
import           Control.Monad.IO.Class                   ( liftIO )
import           Control.Monad.Reader                     ( asks )
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
import           Data.List                                ( find )
import           Data.Maybe                               ( mapMaybe )
import           Data.Text.Lazy                           ( Text
                                                          , pack
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

import           Model.Types                              ( Client
                                                          , ClientContext(..)
                                                          , NoMenu(..)
                                                          , Menu(..)
                                                          )

import           Util                                     ( safeBS )

apiURL :: String
apiURL = "https://heimdallprod.azurewebsites.net/graphql"

graphQLName :: String
graphQLName = "DishOccurrencesByTimeRangeQuery"

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

requestData :: (ToJSON a, ToJSON b) => a -> b -> b -> Value
requestData unitID startDate endDate = object
  [ "query" .= graphQLQuery
  , "operationName" .= graphQLName
  , "variables" .= object
    [ "mealProvidingUnitID" .= unitID
    , "startDate" .= startDate
    , "endDate" .= endDate
    ]
  ]

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

fetchMenu :: Language -> String -> String -> Client (Either NoMenu [Menu])
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
    $   \case
          [] -> Left NoLunch
          xs -> Right xs
    .   mapMaybe (\m -> Menu (variant m) <$> nameOf lang m)
    =<< first
          (SomethingWrong . Just . pack)
          (   parseEither parseResponse
          =<< eitherDecode
          =<< note "Getting data didn't work out as expected." response
          )
