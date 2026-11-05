{-# LANGUAGE TemplateHaskell, BangPatterns, RecordWildCards, OverloadedStrings, LambdaCase #-}

-- | Types and internal functions

module Model.Types where

import           Data.ByteString.Lazy                     ( ByteString )
import           Data.List.NonEmpty                       ( NonEmpty )
import           Data.Text.Lazy                           ( Text )
import           Data.Thyme                               ( defaultTimeLocale
                                                          , formatTime
                                                          , LocalTime
                                                          )
import           Data.Aeson                               ( (.=)
                                                          , object
                                                          , ToJSON
                                                          , toJSON
                                                          )
import           Lens.Micro.Platform                      ( makeLenses )


-- | What to pass to template.
data View = View
  { restaurants :: [Restaurant]
  , day         :: Text
  , date        :: LocalTime
  } deriving (Show)

instance ToJSON View where
  toJSON (View {..}) = object
    [ "restaurants" .= restaurants
    , "day" .= day
    , "date" .= formatTime defaultTimeLocale "%F" date
    ]

-- | One pretty restaurant.
data Restaurant = Restaurant
  { name :: Text
  , url  :: Text
  , menu :: Either NoMenu (NonEmpty Menu)
  } deriving (Show)

instance ToJSON Restaurant where
  toJSON (Restaurant {..}) = object
    [ "name" .= name
    , "url" .= url
    , "menu" .= either toJSON toJSON menu
    ]

data NoMenu
  = NoLunch
  | NMParseError String ByteString -- ^ The parse error. The string we tried to parse.
  | NMExceptionRaised String -- ^ The exception raised.
  deriving (Eq, Show)

instance ToJSON NoMenu where
  toJSON = \case
    NoLunch -> "No lunch"
    NMParseError _ _ -> "Parse error"
    NMExceptionRaised _ -> "Exception raised"

-- | Menu of a restaurant.
-- Title, Body text
data Menu = Menu
  { _mName :: Text
  , _mFood :: Text
  }
  deriving (Eq, Show)

instance ToJSON Menu where
  toJSON (Menu {..}) = object
    [ "name" .= _mName
    , "food" .= _mFood
    ]


makeLenses ''Menu
