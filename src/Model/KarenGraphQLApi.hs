{-# LANGUAGE FlexibleContexts, OverloadedStrings, QuasiQuotes #-}

module Model.KarenGraphQLApi
  ( fetch
  , parse
  , fetchAndCreateRestaurant
  )
where

import           Control.Monad                            ( (>=>) )
import           Control.Monad.Catch                      ( MonadThrow )
import           Control.Monad.IO.Class                   ( MonadIO )
import           Control.Monad.Reader                     ( MonadReader )
import           Data.Aeson                               ( object
                                                          , (.=)
                                                          , encode
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
import           Data.Foldable                            ( find )
import           Data.Text.Lazy                           ( Text
                                                          , unpack
                                                          )
import           Data.Thyme                               ( Day )
import           Data.Thyme.Calendar.WeekDate             ( _mwDay
                                                          , mondayWeek
                                                          )
import           Lens.Micro.Platform                      ( (.~) )
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
        |    name
        |  }
        |}
        ||]

type Language = String

-- | Fetch a menu from Kårens GraphQL API.
-- Parameters: RestaurantUUID, Day.
fetch
  :: (MonadIO m, MonadReader ClientContext m, MonadThrow m)
  => String
  -> Day
  -> m (Either NoMenu BL8.ByteString)
fetch restaurantUUID d = do
  initialRequest <- parseRequest apiURL
  safeBS
    (initialRequest
      { method         = "POST"
      , requestBody    = RequestBodyLBS (encode $ requestData restaurantUUID)
      , requestHeaders = [("Content-Type", "application/json")]
      }
    )
 where
  setDay :: Int -> Day -> Day
  setDay = (mondayWeek . _mwDay .~)

  requestData :: String -> Value
  requestData unitID = object
    [ "query" .= graphQLQuery
    , "operationName" .= ("DishOccurrencesByTimeRangeQuery" :: String)
    , "variables" .= object
      [ "mealProvidingUnitID" .= unitID
      , "startDate" .= show (1 `setDay` d)
      , "endDate" .= show (5 `setDay` d)
      ]
    ]

-- | Parses menus from Kåren's GraphQL API.
-- Parameters: Language, raw data
parse :: Language -> BL8.ByteString -> Either NoMenu [Menu]
parse lang =
  failWithNoMenu eitherDecode
    >=> failWithNoMenu
          (parseEither
            (   withObject "Parse meals"
            $   (.: "data")
            >=> (.: "dishOccurrencesByTimeRange")
            >=> mapM (createMenuParser lang)
            )
          )
    >=> menusToEitherNoLunch
 where
  failWithNoMenu :: Show a => (a -> Either String b) -> a -> Either NoMenu b
  failWithNoMenu action x =
    first (\msg -> NMParseError msg . BL8.pack . show $ x) (action x)

  createMenuParser :: Language -> Value -> Parser Menu
  createMenuParser lang' = withObject "Menu Object" $ \obj ->
    Menu
      <$> (obj .: "dishType" >>= (.: "name"))
      <*> ((obj .: "displayNames") >>= withArray
            "An array of meal names"
            (   mapM
                ( withObject "The name of the meal in many languages"
                $ \o' -> (,) <$> (o' .: "categoryName") <*> (o' .: "name")
                )
            >=> ( maybe (fail $ "Couldn't find the language: " <> show lang')
                        (pure . snd)
                . find ((== lang') . fst)
                )
            )
          )

-- | Parameters: Day, title, tag, uuid
fetchAndCreateRestaurant
  :: (MonadIO m, MonadReader ClientContext m, MonadThrow m)
  => Day
  -> Text
  -> Text
  -> Text
  -> m Restaurant
fetchAndCreateRestaurant day title tag uuid =
  Restaurant
      title
      (  "http://carbonatescreen.azurewebsites.net/menu/week/"
      <> tag
      <> "/"
      <> uuid
      )
    <$> fmap (parse "Swedish" =<<) (fetch (unpack uuid) day)
