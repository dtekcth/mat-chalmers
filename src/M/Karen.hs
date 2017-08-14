{-# LANGUAGE OverloadedStrings #-}
-- |
module M.Karen where

import Data.Aeson (decode)
import Data.Aeson.Types -- (parseMaybe)
import qualified Data.Text.Lazy as T
import Data.Thyme

import M.Types hiding (menu, name, url, day)
import M.KarenJSON
import Util

-- | Get a restaurant that kåren has.
getKaren :: Day -> T.Text -> String -> T.Text -> IO Restaurant
getKaren weekday name restUrl menuUrl = do
  text <- handle' (get' restUrl)
  return $
    Restaurant name menuUrl . maybe (Left SomethingWrong) id $ do
      text' <- text
      val <- decode text'
      res <- parseMaybe (parseMenuForDay weekday) val
      return $ case res of
        Nothing -> Left NoLunch
        Just l -> Right l
