module Model.Wijkanders
  ( getWijkanders
  )
where

import           Control.Arrow                            ( (***)
                                                          , (>>>)
                                                          )
import           Data.Attoparsec.ByteString.Lazy          ( maybeResult
                                                          , parse
                                                          , skip
                                                          , skipMany
                                                          , string
                                                          )
import           Data.ByteString.Lazy                     ( ByteString )
import qualified Data.ByteString.Lazy          as BL
import qualified Data.ByteString.Lazy.Char8    as BL8
import           Data.Maybe                               ( isJust
                                                          , mapMaybe
                                                          )
import           Data.Text.Encoding.Error                 ( ignore )
import           Data.Text.Lazy.Encoding                  ( decodeUtf8With )
import           Data.Thyme                               ( Day
                                                          , gregorian
                                                          , ymdMonth
                                                          , ymdDay
                                                          , _ymdDay
                                                          )
import qualified Data.Word8                    as W8
import           GHC.Exts                                 ( fromString )
import           Lens.Micro.Platform                      ( (%~)
                                                          , (&)
                                                          , view
                                                          )
import           Safe                                     ( atMay )
import           Text.HTML.TagSoup                        ( (~==)
                                                          , (~/=)
                                                          , maybeTagText
                                                          , parseTags
                                                          , partitions
                                                          )
import           Text.HTML.TagSoup.Match                  ( tagText )

import           Model.Types                              ( Menu(..)
                                                          , NoMenu(..)
                                                          )
import           Util                                     ( menusToEitherNoLunch
                                                          , removeWhitespaceTags
                                                          )

hasDate :: Day -> ByteString -> Bool
hasDate d =
  let d' = view gregorian d
  in  isJust . maybeResult . parse
        (skipMany (skip (not . W8.isDigit)) *> string
          (fromString (show (ymdDay d') <> "/" <> show (ymdMonth d')))
        )

-- | getWijkanders will either give you a list of menus or NoLunch.
-- At the moment there is no way to catch parsing errors.
-- We also bet all our money on red 17, and that the maintainers of
-- Wijkander's homepage keep writing dd/mm for every day.
getWijkanders :: Day -> ByteString -> Either NoMenu [Menu]
getWijkanders d =
  parseTags
        -- Take tags from a start date to the next date or to the phrase
        -- "Med reservation", I really hope this hack works longer than the 30th
        -- of april. Brittlebrittlebrittle.
    >>> dropWhile (not . tagText (hasDate d))
    >>> takeWhile
          (not . tagText
            (\s ->
              BL.isPrefixOf (BL8.pack "Med reservation") s || hasDate tomorrow s
            )
          )
    -- The heading is of no use to us.
    >>> drop 1
    >>> dropWhile (~/= "<strong>")
    >>> removeWhitespaceTags
    >>> partitions (~== "<strong>")
    >>> mapMaybe ((maybeTagText =<<) . (`atMay` 1))
    >>> map
          (   BL.break (== W8._colon)
          >>> (decodeUtf8With ignore *** (decodeUtf8With ignore . BL.drop 1))
          >>> uncurry Menu
          )
    >>> menusToEitherNoLunch
  where tomorrow = d & (gregorian . _ymdDay) %~ (+ 1)
