module Model.Karen where

import           Data.Aeson                               ( eitherDecode )
import           Data.Aeson.Types                         ( parseEither )
import           Data.ByteString.Lazy                     ( ByteString )
import           Data.Bifunctor                           ( first )
import           Data.Maybe                               ( fromMaybe )
import           Data.Text.Lazy                           ( Text
                                                          , pack
                                                          )
import           Data.Text.Lazy.Encoding                  ( decodeUtf8 )
import           Data.Thyme                               ( Day )

import           Model.Types                              ( Menu
                                                          , NoMenu(..)
                                                          , Restaurant(..)
                                                          )
import           Model.KarenJSON

-- | Parse a restaurant that Kåren has.
getKaren :: Day -> ByteString -> Either NoMenu [Menu]
getKaren weekday rawBS = maybe (Left NoLunch) (pure . pure) =<< first
  (flip NMParseError rawBS)
  (parseEither (parseMenuForDay weekday) =<< eitherDecode rawBS)

-- | Parse today's menu for a restaurant that Kåren has.
getKarenToday :: ByteString -> Either NoMenu [Menu]
getKarenToday rawBS =
  first (flip NMParseError rawBS)
    $   fmap (concat . snd)
    $   parseEither parseMenus
    =<< eitherDecode rawBS
