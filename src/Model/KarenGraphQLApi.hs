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
                                                          , withArray
                                                          , withObject
                                                          , eitherDecode
                                                          , Value
                                                          )
import           Data.Aeson.Types                         ( Parser
                                                          , parseEither
                                                          )
import           Data.Bifunctor                           ( first )
import qualified Data.ByteString.Lazy.Char8    as BL8
import           Data.List                                ( lookup )
import           Data.Maybe                               ( mapMaybe )
import qualified Data.Text                     as T
import           Data.Text.Lazy                           ( Text
                                                          , unpack
                                                          )
import           GHC.Exts                                 ( toList )
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
        |}
        |
        |fragment MenuDish on Dish {
        |  name
        |}|]

type Language = String

data Meal =
  Meal
    { names   :: [(Language, Text)]
    , variant :: Text
    }
  deriving (Show)

instance FromJSON Meal where
  parseJSON =
    withObject "Meal Descriptor Object" $ \obj ->
      Meal
        <$> ((obj .: "displayNames")
          >>= withArray "An array of meal names" (
            fmap toList . mapM (withObject "The name of the meal in many languages"
              $ \o' -> (,) <$> (o' .: "categoryName") <*> (o' .: "name"))))
        <*> (obj .: "dishType" >>= (.: "name"))


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
    >>= failWithNoMenu
          (parseEither
            ( withObject "Parse meals"
            $ \obj -> obj .: "data" >>= (.: "dishOccurrencesByTimeRange")
            )
          )
    >>= menusToEitherNoLunch
    .   mapMaybe (\m -> Menu (variant m) <$> lookup lang (names m))
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
