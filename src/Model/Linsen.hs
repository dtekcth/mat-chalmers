{-# LANGUAGE FlexibleContexts, OverloadedStrings, LambdaCase #-}
{-# LANGUAGE PatternSynonyms #-}

module Model.Linsen
  (
    parse
  , fetchAndCreateLinsen
  )
where

import           Control.Monad                            ( (>=>)
                                                          , (<=<)
                                                          , zipWithM
                                                          , ap )
import           Control.Monad.Catch                      ( MonadThrow )
import           Control.Monad.IO.Class                   ( MonadIO )
import           Data.Aeson                               ( (.:)
                                                          , withObject
                                                          , Value )
import           Data.Aeson.Types                         ( Parser
                                                          , parseEither )
import           Data.Bifunctor                           ( first )
import qualified Data.ByteString.Lazy.Char8    as BL8
import           Data.Functor                             ( (<&>) )
import           Data.List.Extra                          ( (!?) )
import           Data.Text.Lazy                           ( Text
                                                          , replace
                                                          , strip )
import           Data.Thyme                               ( parseTime
                                                          , TimeLocale (..) )
import           Data.Thyme.Calendar                      ( Day )
import           Data.Thyme.Calendar.WeekDate             ( weekDate
                                                          , _wdDay )
import           Lens.Micro.Platform                      ( (^.) )
import           Network.HTTP.Req
import           Model.Types                              ( NoMenu(..)
                                                          , Menu(..)
                                                          , Restaurant ( Restaurant ) )
import           Util                                     ( menusToEitherNoLunch )

swedishTimeLocale :: TimeLocale
swedishTimeLocale = TimeLocale
        { wDays =
            [ ("Söndag",  "Sön")
            , ("Måndag",  "Mån")
            , ("Tisdag",  "Tis")
            , ("Onsdag",  "Ons")
            , ("Torsdag", "Tors")
            , ("Fredag",  "Fre")
            , ("Lördag",  "Lör")
            ]
        , months =
            [ ("Januari",   "Jan")
            , ("Februari",  "Feb")
            , ("Mars",      "Mar")
            , ("April",     "Apr")
            , ("Maj",       "Maj")
            , ("Juni",      "Jun")
            , ("Juli",      "Jul")
            , ("Augusti",   "Aug")
            , ("September", "Sep")
            , ("Oktober",   "Oct")
            , ("November",  "Nov")
            , ("December",  "Dec")
            ]
        , amPm = ("fm", "em")
        , dateTimeFmt = "%a %f %e %H:%M:%S %Z %Y"
        , dateFmt = "%d/%m/%y"
        , timeFmt = "%H:%M:%S"
        , time12Fmt = "%I:%M:%S %p"
        , knownTimeZones = []
        }

pattern MeatDish, FishDish, VegDish :: Integer
pattern MeatDish = 3
pattern FishDish = 8
pattern VegDish = 13

fetch
  :: (MonadHttp m, MonadIO m, MonadThrow m)
  => m Value  -- ^ A JSON response or horrible crash
fetch =
  req
    GET
    (https "cafe-linsen.se" /: "api" /: "menu")
    NoReqBody
    jsonResponse
    mempty
  <&> responseBody

parse
  :: Day                  -- ^ Day to parse
  -> Value                -- ^ JSON result from `fetch`
  -> Either NoMenu [Menu] -- ^ Either list of parsed `Menu`s or `NoMenu` error
parse day =
    failWithNoMenu
      (parseEither
        (let index = 6 - day ^. weekDate . _wdDay in
        if index `notElem` [1..5] then
            pure . const []
        else
          withObject "Parse meals"
          $   (.: "docs")
          >=> (\case
                Nothing -> fail "Failed to index into days"
                Just v -> pure v) . (!? index)
          >=> (.: "richText")
          >=> (.: "root")
          >=> (.: "children")
          >=> (\v' ->
                 (case v' !? 1 of
                   Nothing -> fail "failed to index into food"
                   Just v -> pure v) >>=
                 withObject "Parse day" (
                  (.: "children")
                  >=> (\case
                          []    -> fail "Failed to index into richtext"
                          (v:_) -> pure v)
                  >=> (.: "text")
                  >=> \s ->
                    case pure day == parseTime swedishTimeLocale "%A %d-%m-%Y" s of
                      True | length v' >= 9 -> pure v'
                           | otherwise      -> pure mempty
                      False                 -> fail "Unable to parse day"))
          >=> menuParser
        )
      )
    >=> menusToEitherNoLunch
 where
  failWithNoMenu :: Show a => (a -> Either String b) -> a -> Either NoMenu b
  failWithNoMenu action x =
    first (\msg -> NMParseError msg . BL8.pack . show $ x) (action x)

  menuParser :: [Value] -> Parser [Menu]
  menuParser = pure . (zip [0 :: Integer ..] >=> \case
                          (MeatDish, vs) -> [vs]
                          (FishDish, vs) -> [vs]
                          (VegDish , vs) -> [vs]
                          _       -> [])
               <=< ap (zipWithM sumFood) tail

  sumFood :: Value -> Value -> Parser Menu
  sumFood a b = Menu <$> getFood a <*> getFood b

  getFood :: Value -> Parser Text
  getFood = withObject "Menu Object"
            $   (.: "children")
            >=> \case
                  [] -> pure mempty
                  vs -> strip . replace "/ " ", "
                        <$> last vs .: "text"

fetchAndCreateLinsen
  :: (MonadHttp m, MonadIO m, MonadThrow m)
  => Day          -- ^ Day
  -> m Restaurant -- ^ Fetched Restaurant
fetchAndCreateLinsen day =
  Restaurant
      "Café Linsen"
      "https://cafe-linsen.se/#menu"
    <$> fmap (parse day) fetch
