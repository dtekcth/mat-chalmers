{-# LANGUAGE FlexibleContexts, OverloadedStrings, QuasiQuotes #-}

module Model.Linsen
  ( fetch
  , parse
  , fetchAndCreateRestaurant
  )
where

import           Control.Monad                            ( (>=>)
                                                          , foldM
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
import           Data.Text.Lazy                           ( Text
                                                          )
import           Data.Thyme.Calendar                      ( Day)
-- import           Lens.Micro.Platform                      ( (^.)
--                                                           , (&)
--                                                           , (%~)
--                                                           , view
--                                                           )
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
    POST
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
        >=> (.: fromString (show (6 - (\(_,_,a) -> a) (toWeekDate day))))
        >=> (.: "richText")
        >=> (.: "root")
        >=> (.: "children")
        >=> (foldM menuParser [] :: [Value] -> Parser [Menu])
        )
      )
    >=> menusToEitherNoLunch
 where
  failWithNoMenu :: Show a => (a -> Either String b) -> a -> Either NoMenu b
  failWithNoMenu action x =
    first (\msg -> NMParseError msg . BL8.pack . show $ x) (action x)

  menuParser :: [Menu] -> Value -> Parser [Menu]
  menuParser o v = (:o) <$> (withObject "Menu Object" $ \_obj ->
    Menu
      <$> undefined
      <*> undefined --TODO: parse
      -- Just needs fields (2,3), & (6,7), & (10,11)
      -- (2,3)   is Meat
      -- (6,7)   is Fish
      -- (10,11) is Vegetarian
    ) v


fetchAndCreateRestaurant
  :: (MonadHttp m, MonadIO m, MonadThrow m)
  => Day          -- ^ Day
  -> Text         -- ^ Title
  -> m Restaurant -- ^ Fetched Restaurant
fetchAndCreateRestaurant day title =
  Restaurant
      title
      "https://plateimpact-screen.azurewebsites.net/menu/week/"
    <$> fmap (parse day) fetch
