{-# LANGUAGE FlexibleContexts, LambdaCase #-}

module Util where

import           Data.ByteString.Lazy                     ( ByteString )
import qualified Data.ByteString.Lazy          as BL
import qualified Data.Word8                    as W8

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
  filter (or . ([not . isTagText, tagText (not . BL.all W8.isSpace)] <*>) . pure)
