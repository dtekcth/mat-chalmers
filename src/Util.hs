{-# LANGUAGE FlexibleContexts, LambdaCase #-}

module Util where

import           Data.ByteString.Lazy                     ( ByteString )
import qualified Data.ByteString.Lazy          as BL
import qualified Data.Word8                    as W8

import           Data.Thyme                               ( TimeLocale (..) )
import           Text.HTML.TagSoup                        ( Tag
                                                          , isTagText
                                                          )
import           Text.HTML.TagSoup.Match                  ( tagText )

import           Model.Types                              ( Menu
                                                          , NoMenu(..)
                                                          )
import           Lens.Micro.Platform

takeNext :: [a] -> [a]
takeNext = take 1 . drop 1

-- | Turn a list of Menu into an `Either NoMenu [Menu]`
menusToEitherNoLunch :: [Menu] -> Either NoMenu [Menu]
menusToEitherNoLunch = \case
  [] -> Left NoLunch
  xs -> Right xs

-- | Remove text tags that only contain whitespace.
removeWhitespaceTags :: [Tag ByteString] -> [Tag ByteString]
removeWhitespaceTags =
  filter (or . ([not . isTagText, tagText (not . BL.all W8.isSpace)] <*>) . pure)

(^.^) :: Monad m => s -> Getting a s a -> m a
(^.^)  = (pure .) . (^.)
infixl 8 ^.^

-- | Locale for Sweden
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
