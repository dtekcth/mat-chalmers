{-# LANGUAGE OverloadedStrings #-}
-- |

module M.Karen where

import           Control.Lens
import qualified Data.Text.Lazy as T
import           Data.Thyme
import           System.Locale (defaultTimeLocale)
import           Text.Taggy
import           Text.Taggy.Lens as TT

import           M.Internal hiding (menu)

-- | Get a restaurant that kåren has.
getKaren :: LocalTime -> T.Text -> String -> IO (Maybe Restaurant)
getKaren date name format =
  handle' (fmap (getRestaurant name) (getAndParse url))
  where url = (formatTime defaultTimeLocale format) date

getRestaurant name tags = Restaurant name today
  where today =
          tags
          ^.. html
          . allNamed (only "item")
          . to menu

menu :: Element -> Menu
menu spec =
  Menu (spec
        ^. TT.elements
        . named (only "title")
        . contents
        . to T.fromStrict)
       (spec
        ^. TT.elements
        . named (only "description")
        . contents
        . to (T.takeWhile (/= '@') . T.fromStrict))
