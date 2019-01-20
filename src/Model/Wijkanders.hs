module Model.Wijkanders
  ( getWijkanders
  )
where

import           Control.Arrow                            ( (***)
                                                          , (>>>)
                                                          )
import           Control.Error.Util                       ( note )
import           Data.Attoparsec.ByteString.Lazy          ( Parser
                                                          , maybeResult
                                                          , parse
                                                          , skip
                                                          , skipMany
                                                          , string
                                                          )
import qualified Data.ByteString               as B
import           Data.ByteString.Lazy                     ( ByteString )
import qualified Data.ByteString.Lazy          as BL
import           Data.Functor                             ( (<&>) )
import           Data.Maybe                               ( fromMaybe
                                                          , isJust
                                                          , mapMaybe
                                                          )
import           Data.Thyme                               ( Day
                                                          , _ymdDay
                                                          , gregorian
                                                          , ymdMonth
                                                          , ymdDay
                                                          )
import qualified Data.Word8                    as W8
import           GHC.Exts                                 ( fromString )
import           Lens.Micro.Platform                      ( (%~)
                                                          , (&)
                                                          , view
                                                          )
import           Safe                                     ( atMay )
import qualified Data.Text.Lazy                as T
import           Data.Text.Lazy.Encoding                  ( decodeUtf8 )
import           Text.HTML.TagSoup                        ( (~==)
                                                          , Tag
                                                          , isTagText
                                                          , maybeTagText
                                                          , parseTags
                                                          , partitions
                                                          )
import           Text.HTML.TagSoup.Match                  ( tagClose
                                                          , tagText
                                                          )

import           Model.Types                              ( Menu(..)
                                                          , NoMenu(..)
                                                          )
import           Util                                     ( menusToEitherNoLunch
                                                          )

-- | Remove text tags that only contain whitespace.
removeWhitespaceTags :: [Tag ByteString] -> [Tag ByteString]
removeWhitespaceTags =
  filter (\t -> not (isTagText t) || tagText (not . BL.all W8.isSpace) t)

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
        -- Take tags from a start date to the end of that paragraph.
    >>> dropWhile (not . tagText (hasDate d))
    >>> takeWhile (not . tagClose (== fromString "p"))
    -- The heading is of no use to us.
    >>> drop 1
    >>> removeWhitespaceTags
    >>> partitions (~== "<strong>")
    >>> mapMaybe ((maybeTagText =<<) . (`atMay` 1))
    >>> map
          (   BL.break (== W8._colon)
          >>> (decodeUtf8 *** (decodeUtf8 . BL.drop 1))
          >>> uncurry Menu
          )
    >>> menusToEitherNoLunch
