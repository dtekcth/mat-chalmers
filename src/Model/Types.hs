{-# LANGUAGE FlexibleContexts, GeneralizedNewtypeDeriving #-}

-- | Types and internal functions

module Model.Types where

import           Control.Monad.Catch                      ( MonadCatch
                                                          , MonadThrow
                                                          )
import           Control.Monad.Reader                     ( MonadIO
                                                          , MonadReader
                                                          , ReaderT
                                                          )
import           Data.Text.Lazy                           ( Text )
import           Data.Thyme                               ( LocalTime )
import           Network.HTTP.Client                      ( Manager )

import           Config                                   ( Config )


newtype ClientT m r a = ClientT { runClientT :: ReaderT r m a }
  deriving (Functor, Applicative, Monad, MonadCatch, MonadReader r, MonadIO, MonadThrow)

data ClientContext = ClientContext { ccCfg :: Config, ccManager :: Manager }

type Client = ClientT IO ClientContext

-- | What to pass to template.
data View = View
  { restaurants :: [Restaurant]
  , day         :: Text
  , date        :: LocalTime
  } deriving (Eq, Show)

-- | One pretty restaurant.
data Restaurant = Restaurant
  { name :: Text
  , url  :: Text
  , menu :: Either NoMenu [Menu]
  } deriving (Eq, Show)

data NoMenu
  = NoLunch
  | SomethingWrong (Maybe Text)
  deriving (Eq, Show)

-- | Menu of a restaurant.
data Menu = Menu Text Text
  deriving (Eq, Show)
