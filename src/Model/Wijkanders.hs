{-# LANGUAGE LambdaCase, OverloadedStrings  #-}
module Model.Wijkanders
  ( fetchAndCreateWijkanders
  )
where

import           Control.Arrow                            ( (***)
                                                          , (&&&)
                                                          , (>>>)
                                                          )
import           Control.Monad                            ( (<=<) )
import           Control.Monad.Catch                      ( MonadThrow )
import           Control.Monad.IO.Class                   ( MonadIO (liftIO) )
import           Data.Attoparsec.ByteString.Lazy          ( maybeResult
                                                          , parse
                                                          , skip
                                                          , skipMany
                                                          , string
                                                          , takeWhile1
                                                          )
import           Data.ByteString.Lazy                     ( ByteString )
import           Data.Text.Lazy                           ( pack )
import qualified Data.ByteString.Char8         as B8
import qualified Data.ByteString.Lazy          as BL
import qualified Data.ByteString.Lazy.Char8    as BL8
import           Data.Functor                             ( (<&>) )
import           Data.Maybe                               ( mapMaybe )
import           Data.Text.Encoding.Error                 ( ignore )
import           Data.Text.Encoding                       ( encodeUtf8 )
import           Data.Text.Lazy.Encoding                  ( decodeUtf8With )
import           Data.Thyme                               ( Day
                                                          , Days
                                                          , Months
                                                          , YearMonthDay(..)
                                                          , gregorian
                                                          , gregorianValid
                                                          , ymdMonth
                                                          , ymdDay
                                                          )
import qualified Data.Word8                    as W8
import           Lens.Micro.Platform                      ( view )
import           Network.Wreq                             ( get
                                                          , responseBody )
import           Safe                                     ( atMay )
import           Text.HTML.TagSoup                        ( (~==)
                                                          , (~/=)
                                                          , maybeTagText
                                                          , parseTags
                                                          , partitions
                                                          , Tag(..)
                                                          )
import           Text.HTML.TagSoup.Match                  ( tagText )

import           Model.Types                              ( Menu(..)
                                                          , NoMenu(..), Restaurant (Restaurant)
                                                          )
import           Util                                     ( removeWhitespaceTags
                                                          , (^.^) )

wijkandersAPIURL :: String
wijkandersAPIURL = "https://www.wijkanders.se/restaurangen"

-- | Looks for strings looking like dates, dd/mm where d and m are digits.
-- ..and gives them in another order to play nice with the
-- YearMonthDay constructor.
hasDate :: ByteString -> Maybe (Months, Days)
hasDate = maybeResult . parse (flip (,) <$> parseDay <*> parseMonth)
 where
  parseDay      = skipMany (skip (not . W8.isDigit)) *> integerParser
  parseMonth    = string (encodeUtf8 "/") *> integerParser
  integerParser = fmap (read . B8.unpack) (takeWhile1 W8.isDigit)

-- | getWijkanders will either give you a list of menus or NoLunch.
-- At the moment there is no way to catch parsing errors.
-- We also bet all our money on red 17, and that the maintainers of
-- Wijkander's homepage keep writing dd/mm for every day.
getWijkanders :: Day -> ByteString -> Either NoMenu [Menu]
getWijkanders d b = go b
 where
  go = parseTags
    -- Take tags from a start date to the next parsable date or to the
    -- phrase "Med reservation".
    >>> dropWhile
          (not . tagText
            (((pure . (ymdMonth &&& ymdDay) . view gregorian) d ==) . hasDate)
          )
    >>> takeWhile
          (not . tagText
           (or . ([
                    BL.isPrefixOf (BL8.pack "Med reservation")
                  , maybe False (> d) .
                    ( gregorianValid
                      . uncurry (YearMonthDay $ ymdYear $ view gregorian d)
                      <=< hasDate)
                  ] <*>) . pure)
          )
    >>> \case
      [] -> Left NoLunch -- No lunch in the case that we cannot parse the date
      a  -> parseWijkanderLunch a

  parseWijkanderLunch :: [Tag ByteString] -> Either NoMenu [Menu]
  parseWijkanderLunch =
        -- The heading is of no use to us.
        drop 1
        >>> dropWhile (~/= ("<strong>" :: String))
        >>> removeWhitespaceTags
        >>> partitions (~== ("<strong>" :: String))
        >>> mapMaybe (maybeTagText <=< (`atMay` 1))
        >>> map
              (   BL.break (== W8._colon)
              >>> (   decodeUtf8With ignore
                  *** (decodeUtf8With ignore . BL.dropWhile W8.isSpace . BL.drop
                        1
                      )
                  )
              >>> uncurry Menu
              )
        >>> \case
              [] -> Left (NMParseError "Wijkanders failed" b)
              xs -> Right xs

fetchAndCreateWijkanders
  :: (MonadIO m, MonadThrow m)
  => Day
  -> m Restaurant
fetchAndCreateWijkanders day =
    liftIO (get wijkandersAPIURL) >>=
    (^.^ responseBody) <&>
      Restaurant "Wijkanders" (pack wijkandersAPIURL) . getWijkanders day
