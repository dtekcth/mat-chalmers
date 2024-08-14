{-# LANGUAGE FlexibleContexts, OverloadedStrings, QuasiQuotes #-}

module Model.Karen
  ( fetch
  , parse
  , fetchAndCreateRestaurant
  )
where

import           Control.Monad                            ( (>=>), filterM )
import           Control.Monad.Catch                      ( MonadThrow )
import           Control.Monad.IO.Class                   ( MonadIO )
import           Data.Aeson                               ( object
                                                          , (.=)
                                                          , (.:)
                                                          , withArray
                                                          , withObject
                                                          , Value
                                                          )
import           Data.Aeson.Types                         ( Parser
                                                          , parseEither
                                                          )
import           Data.Bifunctor                           ( first )
import qualified Data.ByteString.Lazy.Char8    as BL8
import           Data.Foldable                            ( find )
import           Data.Functor                             ( (<&>) )
import           Data.Text.Lazy                           ( Text
                                                          , unpack
                                                          )
import           Data.Thyme.Calendar                      ( Day
                                                          , showGregorian
                                                          )
import           Network.HTTP.Req
import           Text.Heredoc                             ( str )

import           Model.Types                              ( NoMenu(..)
                                                          , Menu(..)
                                                          , mFood
                                                          , Restaurant
                                                            ( Restaurant
                                                            )
                                                          )
import           Util                                     ( menusToEitherNoLunch
                                                          , (^.^))


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
  :: (MonadHttp m, MonadIO m, MonadThrow m)
  => String   -- ^ RestaurantUUID
  -> Day      -- ^ Day
  -> m Value  -- ^ A JSON response or horrible crash
fetch restaurantUUID day =
  req
    POST
    (https "plateimpact-heimdall.azurewebsites.net" /: "graphql")
    (ReqBodyJson requestData)
    jsonResponse
    mempty
  <&> responseBody

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
  -> Value                -- ^ JSON result from `fetch`
  -> Either NoMenu [Menu] -- ^ Either list of parsed `Menu`s or `NoMenu` error
parse lang =
    failWithNoMenu
      (parseEither
        (   withObject "Parse meals"
        $   (.: "data")
        >=> (.: "dishOccurrencesByTimeRange")
        >=> mapM menuParser
        )
      )
    >=> filterM (((/= "stängt") <$>) . (^.^ mFood))
    >=> menusToEitherNoLunch
 where
  failWithNoMenu :: Show a => (a -> Either String b) -> a -> Either NoMenu b
  failWithNoMenu action x =
    first (\msg -> NMParseError msg . BL8.pack . show $ x) (action x)

  menuParser :: Value -> Parser Menu
  menuParser = withObject "Menu Object" $ \obj ->
    Menu
      <$> (obj .: "dishType" >>= maybe (pure "Unknown menu") (.: "name"))
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
  :: (MonadHttp m, MonadIO m, MonadThrow m)
  => Day          -- ^ Day
  -> Text         -- ^ Title
  -> Text         -- ^ Tag
  -> Text         -- ^ RestaurantUUID
  -> m Restaurant -- ^ Fetched Restaurant
fetchAndCreateRestaurant day title tag uuid =
  Restaurant
      title
      (  "https://plateimpact-screen.azurewebsites.net/menu/week/"
      <> tag
      <> "/"
      <> uuid
      )
    <$> fmap (parse "Swedish") (fetch (unpack uuid) day)
