{-# LANGUAGE FlexibleContexts, OverloadedStrings, LambdaCase  #-}

module Model.Linsen
  (
    fetchAndCreateLinsen
  )
where

import           Control.Monad                            ( (>=>)
                                                          , (<=<)
                                                          , foldM
                                                          , zipWithM
                                                          , ap
                                                          )
import           Control.Monad.Catch                      ( MonadThrow )
import           Control.Monad.IO.Class                   ( MonadIO )
import           Data.Aeson                               ( -- object
                                                          -- , (.=)
                                                          (.:)
                                                          -- , withArray
                                                          , withObject
                                                          , Value
                                                          )
import           Data.Aeson.Key                           ( fromString )
import           Data.Aeson.Types                         ( Parser
                                                          , parseEither
                                                          )
import           Data.Bifunctor                           ( first )
import qualified Data.ByteString.Lazy.Char8    as BL8
import           Data.Functor                             ( (<&>) )
import           Data.Text.Lazy                           ( Text )
import           Data.Thyme.Calendar                      ( Day )
import           Network.HTTP.Req
-- import           Text.Heredoc                             ( str )
import           Model.Types                              ( NoMenu(..)
                                                          , Menu(..)
                                                          , Restaurant
                                                            ( Restaurant
                                                            )
                                                          )
import           Util                                     ( menusToEitherNoLunch )
import           Data.Thyme.Calendar.WeekDate             ( toWeekDate )

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
        (   withObject "Parse meals"
        $   (.: "docs")
        >=> (pure . (!! (6 - (\(_,_,a) -> a) (toWeekDate day))))
        >=> (.: "richText")
        >=> (.: "root")
        >=> (.: "children")
        >=> menuParser
        )
      )
    >=> menusToEitherNoLunch
 where
  failWithNoMenu :: Show a => (a -> Either String b) -> a -> Either NoMenu b
  failWithNoMenu action x =
    first (\msg -> NMParseError msg . BL8.pack . show $ x) (action x)

  menuParser :: [Value] -> Parser [Menu]
  menuParser =  pure . concatMap f . zip [0..] <=< ap (zipWithM sumFood) tail

  f :: (Int, Text) -> [Menu]
  f = (\case
        (2 ,a) -> [Menu "Kött" a]
        (6 ,a) -> [Menu "Fisk" a]
        (10,a) -> [Menu  "Veg" a]
        _      -> [])

  sumFood :: Value -> Value -> Parser Text
  sumFood a b = getFood a <> pure " " <> getFood b

  getFood :: Value -> Parser Text
  getFood = withObject "Menu Object"
            $   (.: "children")
            >=> pure . last
            >=> (.: "text")

fetchAndCreateLinsen
  :: (MonadHttp m, MonadIO m, MonadThrow m)
  => Day          -- ^ Day
  -> m Restaurant -- ^ Fetched Restaurant
fetchAndCreateLinsen day =
  Restaurant
      "Café Linsen"
      "https://plateimpact-screen.azurewebsites.net/menu/week/"
    <$> fmap (parse day) fetch
