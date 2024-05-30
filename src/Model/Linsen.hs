{-# LANGUAGE FlexibleContexts, OverloadedStrings, LambdaCase  #-}

module Model.Linsen
  (
    fetchAndCreateLinsen
  )
where

import           Control.Monad                            ( (>=>)
                                                          , (<=<)
                                                          , zipWithM
                                                          , ap
                                                          )
import           Control.Monad.Catch                      ( MonadThrow )
import           Control.Monad.IO.Class                   ( MonadIO )
import           Data.Aeson                               ( (.:)
                                                          , withObject
                                                          , Value
                                                          )
import           Data.Aeson.Types                         ( Parser
                                                          , parseEither
                                                          )
import           Data.Bifunctor                           ( first )
import qualified Data.ByteString.Lazy.Char8    as BL8
import           Data.Functor                             ( (<&>) )
import           Data.Text.Lazy                           ( Text
                                                          , replace )
import           Data.Thyme.Calendar                      ( Day )
import           Network.HTTP.Req
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
  menuParser = pure . (zip [0 :: Integer ..] >=> \case
                          (2 ,vs) -> [vs]
                          (6 ,vs) -> [vs]
                          (10,vs) -> [vs]
                          _       -> []) <=< ap (zipWithM sumFood) tail

  sumFood :: Value -> Value -> Parser Menu
  sumFood a b = Menu <$> getFood a <*> getFood b

  getFood :: Value -> Parser Text
  getFood = withObject "Menu Object"
            $   (.: "children")
            >=> \case
                  [] -> pure mempty
                  vs -> last vs .: "text"
                    <&> replace "/ " ", " --TODO: replace / with ,

fetchAndCreateLinsen
  :: (MonadHttp m, MonadIO m, MonadThrow m)
  => Day          -- ^ Day
  -> m Restaurant -- ^ Fetched Restaurant
fetchAndCreateLinsen day =
  Restaurant
      "Café Linsen"
      "https://plateimpact-screen.azurewebsites.net/menu/week/"
    <$> fmap (parse day) fetch
