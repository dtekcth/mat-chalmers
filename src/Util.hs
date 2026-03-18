{-# LANGUAGE FlexibleContexts, LambdaCase #-}

module Util where

import           Data.List.NonEmpty                       ( NonEmpty(..) )
import           Data.Thyme                               ( TimeLocale (..) )
import           Effectful                                ( Eff )
import           Effectful.Exception                      ( SomeException )
import           Model.Types                              ( Menu
                                                          , NoMenu(..)
                                                          )
import           Lens.Micro.Platform

menusToEitherNoLunch :: [Menu] -> Either NoMenu (NonEmpty Menu)
menusToEitherNoLunch = \case
  [] -> Left NoLunch
  (x:xs) -> Right (x :| xs)


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

-- If something raises an exception while fetching and parsing data, default handler for saving said exception.
networkExceptionHandler :: (SomeException -> Eff es (Either NoMenu (NonEmpty Menu)))
networkExceptionHandler = pure . Left . NMExceptionRaised . show
