{-# LANGUAGE FlexibleContexts, LambdaCase #-}

module Util where

import           Data.ByteString.Lazy                     ( ByteString )
import qualified Data.ByteString.Lazy          as BL
import qualified Data.Word8                    as W8
import           Data.Thyme.Calendar

import           Text.HTML.TagSoup                        ( Tag
                                                          , isTagText
                                                          )
import           Text.HTML.TagSoup.Match                  ( tagText )

import           Model.Types                              ( Menu
                                                          , NoMenu(..)
                                                          )

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
  filter (\t -> not (isTagText t) || tagText (not . BL.all W8.isSpace) t)

-- |  Increase the date by one with and handles overflow
--    I cannot believe that this isn't in the library
nextDay :: YearMonthDay -> YearMonthDay
nextDay (YearMonthDay y m d)
  | d < len   = YearMonthDay  y       m      (d + 1)
  | m < 12    = YearMonthDay  y      (m + 1)  1
  | otherwise = YearMonthDay (y + 1)  1       1
  where
      len = gregorianMonthLength y m
