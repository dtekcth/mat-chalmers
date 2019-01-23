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
                                                          , NoMenu(..)
                                                          )
import           Util                                     ( menusToEitherNoLunch
                                                          , removeWhitespaceTags
                                                          )

days :: DayOfWeek -> Either NoMenu ByteString
days = \case
  0 -> pure $ fromString "M\195\165ndag"
  1 -> pure $ fromString "Tisdag"
  2 -> pure $ fromString "Onsdag"
  3 -> pure $ fromString "Torsdag"
  4 -> pure $ fromString "Fredag"
  5 -> pure $ fromString "L\195\182rdag"
  6 -> pure $ fromString "S\195\182ndag"
  s -> Left $ NMParseError
    "The week only has seven days numbered 0-6."
    (fromString $ "This is the day we got: " <> show s)

-- | Get Einstein menu
getEinstein :: DayOfWeek -> ByteString -> Either NoMenu [Menu]
getEinstein d s = days d >>= go s
 where
  go d' =
    parseTags
      >>> removeWhitespaceTags
      >>> dropWhile (not . tagText (== d'))
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
