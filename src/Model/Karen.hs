module Model.Karen where

import           Data.Aeson                               ( eitherDecode )
import           Data.Aeson.Types                         ( parseEither )
import           Data.ByteString.Lazy                     ( ByteString )
import           Data.Bifunctor                           ( first )
import           Data.Thyme                               ( Day )

import           Model.Types                              ( Menu
                                                          , NoMenu(..)
                                                          )
import           Model.KarenJSON                          ( parseMenuForDay
                                                          , parseMenus
                                                          )
import           Util                                     ( menusToEitherNoLunch
                                                          )

-- | Parse a restaurant that Kåren has.
getKaren :: Day -> ByteString -> Either NoMenu [Menu]
getKaren weekday rawBS = maybe (Left NoLunch) (pure . pure) =<< first
  (flip NMParseError rawBS)
  (parseEither (parseMenuForDay weekday) =<< eitherDecode rawBS)

-- | Parse today's menu for a restaurant that Kåren has.
getKarenToday :: ByteString -> Either NoMenu [Menu]
getKarenToday rawBS = menusToEitherNoLunch =<< first
  (flip NMParseError rawBS)
  (concat . snd <$> (parseEither parseMenus =<< eitherDecode rawBS))
