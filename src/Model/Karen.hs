{-# LANGUAGE OverloadedStrings #-}
-- |
module Model.Karen where

import           Data.Aeson (decode)
import           Data.Aeson.Types -- (parseMaybe)
import           Data.ByteString.Lazy (ByteString)
import           Data.Text.Lazy (Text)
import           Data.Text.Lazy.Encoding (decodeUtf8)
import           Data.Thyme

import           Model.Types hiding (menu, name, url, day)
import           Model.KarenJSON

-- | Get a restaurant that kåren has.
getKaren :: Day -> Text -> Text -> Maybe ByteString -> Restaurant
getKaren weekday name menuUrl text =
  Restaurant name menuUrl .
  maybe (Left (SomethingWrong (fmap decodeUtf8 text))) id $ do
    text' <- text
    val <- decode text'
    res <- parseMaybe (parseMenuForDay weekday) val
    return $ case res of
      Nothing -> Left NoLunch
      Just l -> Right (concat l)

getKarenToday :: Text -> Text -> Maybe ByteString -> Restaurant
getKarenToday name menuUrl text =
  Restaurant name menuUrl . maybe (Left (SomethingWrong (fmap decodeUtf8 text))) id $ do
    text' <- text
    val <- decode text'
    (_, l) <- parseMaybe (parseMenus) val
    return $ Right (concat l)
