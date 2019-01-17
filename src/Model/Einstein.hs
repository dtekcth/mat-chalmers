{-# LANGUAGE FlexibleContexts, LambdaCase #-}
-- | Get daily menu for Einstein

module Model.Einstein where

import           Control.Arrow                            ( (>>>) )
import           Data.ByteString.Lazy                     ( ByteString )
import           Data.Maybe                               ( mapMaybe )
import           Data.Text.Encoding.Error                 ( ignore )
import           Data.Text.Lazy.Encoding                  ( decodeUtf8With )
import           Data.Thyme.Calendar.WeekDate             ( DayOfWeek )
import           GHC.Exts                                 ( fromString )
import           Text.HTML.TagSoup                        ( (~/=)
                                                          , maybeTagText
                                                          , parseTags
                                                          )
import           Text.HTML.TagSoup.Match                  ( anyAttr
                                                          , tagOpen
                                                          , tagText
                                                          )

import           Model.Types                              ( Menu(..)
                                                          , NoMenu
                                                          )
import           Util                                     ( menusToEitherNoLunch
                                                          , removeWhitespaceTags
                                                          )

days :: DayOfWeek -> ByteString
days = \case
  0 -> fromString "M\195\165ndag"
  1 -> fromString "Tisdag"
  2 -> fromString "Onsdag"
  3 -> fromString "Torsdag"
  4 -> fromString "Fredag"
  5 -> fromString "L\195\182rdag"
  6 -> fromString "S\195\182ndag"

-- | Get Einstein menu
getEinstein :: DayOfWeek -> ByteString -> Either NoMenu [Menu]
getEinstein d =
  parseTags
    >>> removeWhitespaceTags
    >>> dropWhile (not . tagText (== days d))
    >>> takeWhile (~/= "<section>")
    >>> takeWhile
          (not . tagOpen
            (const True)
            (anyAttr ((fromString "class", fromString "serif_regular") ==))
          )
    >>> mapMaybe maybeTagText
    >>> drop 1
    >>> map (Menu (fromString "") . decodeUtf8With ignore)
    >>> menusToEitherNoLunch
