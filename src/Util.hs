{-# LANGUAGE FlexibleContexts , LambdaCase #-}

module Util where

import           Control.Exception                        ( try )
import           Control.Monad.Catch                      ( MonadCatch
                                                          , MonadThrow
                                                          )
import           Control.Monad.Reader                     ( MonadReader
                                                          , ReaderT
                                                          , asks
                                                          )
import           Control.Monad.Trans                      ( MonadIO
                                                          , liftIO
                                                          )
import           Data.Bifunctor                           ( bimap )
import           Data.ByteString.Lazy                     ( ByteString )
import           Data.Text.Lazy                           ( Text
                                                          , pack
                                                          )
import           Data.Text.Lazy.Encoding                  ( decodeUtf8 )
import           Network.HTTP.Client                      ( HttpException
                                                          , Request
                                                          , httpLbs
                                                          , parseRequest
                                                          , responseBody
                                                          )

import           Model.Types                              ( ClientContext(..)
                                                          , Menu
                                                          , NoMenu(..)
                                                          , Restaurant(..)
                                                          )

takeNext :: [a] -> [a]
takeNext = take 1 . drop 1

safeGet
  :: (MonadIO m, MonadReader ClientContext m, MonadThrow m)
  => String
  -> m (Either NoMenu Text)
safeGet = (fmap . fmap) decodeUtf8 . safeGetBS

safeBS
  :: (MonadIO m, MonadReader ClientContext m, MonadThrow m)
  => Request
  -> m (Either NoMenu ByteString)
safeBS r = do
  m <- asks ccManager
  liftIO (fmap (bimap NMHttp responseBody) (handle' (liftIO (httpLbs r m))))

safeGetBS
  :: (MonadIO m, MonadReader ClientContext m, MonadThrow m)
  => String
  -> m (Either NoMenu ByteString)
safeGetBS = (=<<) safeBS . parseRequest

-- | Handler for HttpExceptions
handle' :: IO a -> IO (Either HttpException a)
handle' = try

-- | Turn a list of Menu into an `Either NoMenu [Menu]`
menusToEitherNoLunch :: [Menu] -> Either NoMenu [Menu]
menusToEitherNoLunch = \case
  [] -> Left NoLunch
  xs -> Right xs
