{-# LANGUAGE FlexibleContexts, OverloadedStrings, QuasiQuotes #-}

module Model.Karen
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
import           Data.Thyme.Calendar                      ( Day
                                                          , showGregorian
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
apiURL = "https://plateimpact-heimdall.azurewebsites.net/graphql"

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
fetch
  :: (MonadIO m, MonadReader ClientContext m, MonadThrow m)
  => String                           -- ^ RestaurantUUID
  -> Day                              -- ^ Day
  -> m (Either NoMenu BL8.ByteString) -- ^ Either a bytestring payload or a NoMenu error
fetch restaurantUUID day = do
  initialRequest <- parseRequest apiURL
  safeBS
    (initialRequest { method         = "POST"
                    , requestBody    = RequestBodyLBS $ encode requestData
                    , requestHeaders = [("Content-Type", "application/json")]
                    }
    )
 where
  requestData = object
    [ "query" .= graphQLQuery
    , "operationName" .= ("DishOccurrencesByTimeRangeQuery" :: String)
    , "variables" .= object
      [ "mealProvidingUnitID" .= restaurantUUID
      , "startDate" .= showGregorian day
      , "endDate" .= showGregorian day
      ]
    ]

-- | Parses menus from Kåren's GraphQL API.
parse
  :: Language             -- ^ Language
  -> BL8.ByteString       -- ^ Bytestring payload from fetch
  -> Either NoMenu [Menu] -- ^ Either list of parsed Menu's or NoMenu error
parse lang =
  failWithNoMenu eitherDecode
    >=> failWithNoMenu
          (parseEither
            (   withObject "Parse meals"
            $   (.: "data")
            >=> (.: "dishOccurrencesByTimeRange")
            >=> mapM menuParser
            )
          )
    >=> menusToEitherNoLunch
 where
  failWithNoMenu :: Show a => (a -> Either String b) -> a -> Either NoMenu b
  failWithNoMenu action x =
    first (\msg -> NMParseError msg . BL8.pack . show $ x) (action x)

  menuParser :: Value -> Parser Menu
  menuParser = withObject "Menu Object" $ \obj ->
    Menu
      <$> (obj .: "dishType" >>= (.: "name"))
      <*> ((obj .: "displayNames") >>= withArray
            "An array of meal names"
            (   mapM
                ( withObject "The name of the meal in many languages"
                $ \o' -> (,) <$> (o' .: "categoryName") <*> (o' .: "name")
                )
            >=> ( maybe (fail $ "Couldn't find the language: " <> show lang)
                        (pure . snd)
                . find ((== lang) . fst)
                )
            )
          )

-- | Fetch a restaurant from Kåren's GraphQL API
fetchAndCreateRestaurant
  :: (MonadIO m, MonadReader ClientContext m, MonadThrow m)
  => Day          -- ^ Day
  -> Text         -- ^ Title
  -> Text         -- ^ Tag
  -> Text         -- ^ RestaurantUUID
  -> m Restaurant -- ^ Fetched Restaurant
fetchAndCreateRestaurant day title tag uuid =
  Restaurant
      title
      (  "http://carbonatescreen.azurewebsites.net/menu/week/"
      <> tag
      <> "/"
      <> uuid
      )
    <$> fmap (parse "Swedish" =<<) (fetch (unpack uuid) day)
