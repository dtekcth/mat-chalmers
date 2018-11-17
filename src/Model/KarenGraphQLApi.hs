{-# LANGUAGE OverloadedStrings #-}
module Model.KarenGraphQLApi (
 fetchMenu, Date(Date), Err, transformMenu, Language(..)
)  where

import Network.HTTP.Client
import Network.HTTP.Client.TLS
import Data.Aeson (object, (.=), encode, FromJSON(parseJSON), ToJSON(toJSON), (.:), withObject, Object, withText, eitherDecode, Value(String), decode)
import Data.Aeson.Types (Parser, parseEither)
import Data.Text (Text, pack)
import Data.List (find)
import Data.Maybe (fromMaybe, catMaybes)
import qualified Data.Text.Lazy as LT
import Data.List (intercalate)
import qualified Data.ByteString.Lazy.Internal as LBS
import Model.Types (NoMenu (NoLunch, SomethingWrong), Menu (Menu))

apiURL :: String
apiURL = "https://heimdallprod.azurewebsites.net/graphql"

graphQLName :: String
graphQLName = "DishOccurrencesByTimeRangeQuery"
graphQLQuery :: String
graphQLQuery =
  "query DishOccurrencesByTimeRangeQuery($mealProvidingUnitID: String, $startDate: String, $endDate: String) {\n\
  \  dishOccurrencesByTimeRange(mealProvidingUnitID: $mealProvidingUnitID, startDate: $startDate, endDate: $endDate) {\n\
  \    ...MenuDishOccurrence\n\
  \  }\n\
  \}\n\
  \\n\
  \fragment MenuDishOccurrence on DishOccurrence {\n\
  \  displayNames {\n\
  \    name\n\
  \    categoryName\n\
  \  }\n\
  \  startDate\n\
  \  dishType {\n\
  \    name\n\
  \  }\n\
  \  dish {\n\
  \    ...MenuDish\n\
  \  }\n\
  \  mealProvidingUnit {\n\
  \    mealProvidingUnitName\n\
  \    id\n\
  \  }\n\
  \}\n\
  \\n\
  \fragment MenuDish on Dish {\n\
  \  name\n\
  \  price\n\
  \  recipes {\n\
  \    portions\n\
  \    name\n\
  \    allergens {\n\
  \      id\n\
  \      name\n\
  \      imageUrl\n\
  \      sortOrder\n\
  \    }\n\
  \  }\n\
  \}"

requestData :: (ToJSON a, ToJSON b) => a -> b -> b -> Value
requestData unitID startDate endDate =
  object
    [ "query" .= graphQLQuery
    , "operationName" .= graphQLName
    , "variables" .=
      object
        [ "mealProvidingUnitID" .= unitID
        , "startDate" .= startDate
        , "endDate" .= endDate
        ]
    ]

data Language
  = Swe
  | Eng
  deriving (Show, Eq)

instance FromJSON Language where
  parseJSON =
    withText "asdaf" $ \str -> case str of
      "Swedish" -> return Swe
      "English" -> return Eng

data MealName =
  MealName
    { name :: String
    , language :: Language
    }
  deriving (Show)

instance FromJSON MealName where
  parseJSON =
    withObject "asdf" $ \obj ->
      MealName
        <$> obj .: "name"
        <*> obj .: "categoryName"


data Meal =
  Meal
    { names :: [MealName]
    , unit :: String
    , variant :: String
    }
  deriving (Show)

--deepLookup :: [Text] -> Object -> Parser a
deepLookup [prop]       obj =
  obj .: prop
deepLookup (prop:props) obj =
  obj .: prop >>= deepLookup props

instance FromJSON Meal where
  parseJSON =
    withObject "Meal Descriptor Object" $ \obj ->
      Meal
        <$> obj .: "displayNames"
        <*> deepLookup ["mealProvidingUnit", "mealProvidingUnitName"] obj
        <*> deepLookup ["dishType", "name"] obj

type Year = Int
type Month = Int
type Day = Int
data Date = Date Year Month Day

instance Show Date where
  show (Date y m d) = intercalate "-" [show y, assureTwo m, assureTwo d]
    where
      assureTwo n = if n < 10 then '0' : show n else show n

instance ToJSON Date where
  toJSON = String . pack . show


data Err =
  Err
    { message :: String
    , reqBody :: LBS.ByteString
    , resturantId :: String
    , date :: String
    }
  deriving (Show)

leftBind mapper either =
  case either of
    Left val ->
      mapper val
    Right val ->
      Right val

rightBind mapper either =
  case either of
    Right val ->
      mapper val
    Left val ->
      Left val

parseResponse :: Value -> Parser [Meal]
parseResponse =
  withObject "ag" (deepLookup ["data", "dishOccurrencesByTimeRange"])

nameOf :: Language -> Meal -> Maybe LT.Text
nameOf lang meal =
  find ((==lang) . language) (names meal) >>= return . LT.pack . name

transformMenu :: Language -> Either Err [Meal] -> Either NoMenu [Menu]
transformMenu lang mealData =
  case mealData of
    Left err ->
      Left $ SomethingWrong $ Just $ LT.pack $ message err
    Right meals ->
      if length menus == 0
      then Left NoLunch
      else Right menus
      where
        menus =
          catMaybes
          $ map (\m -> nameOf lang m >>= return.(Menu(LT.pack(variant m))))
          $ meals


fetchMenu :: String -> String -> IO (Either Err [Meal])
fetchMenu restaurantUUID day = do
  manager <- newManager tlsManagerSettings
  initialRequest <- parseRequest apiURL
  let request =
        initialRequest
          { method = "POST"
          , requestBody = RequestBodyLBS (encode $ requestData restaurantUUID day day)
          , requestHeaders =
            [ ("Content-Type", "application/json")
            ]
          }

  response <- httpLbs request manager
  let rawData = responseBody response
  let mkErr = \msg ->
        Err
          { message = msg
          , reqBody = rawData
          , resturantId = restaurantUUID
          , date = day
          }

  return
    $ leftBind (Left . mkErr)
    $ rightBind (parseEither parseResponse)
    $ eitherDecode rawData

