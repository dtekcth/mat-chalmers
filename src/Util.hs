{-# LANGUAGE FlexibleContexts, LambdaCase #-}

module Util where

import           Control.Exception                        ( try )
import           Control.Monad.Catch                      ( MonadThrow )
import           Control.Monad.Reader                     ( MonadReader
                                                          , asks
                                                          )
import           Control.Monad.Trans                      ( MonadIO
                                                          , liftIO
                                                          )
import           Data.Bifunctor                           ( bimap )
import           Data.ByteString.Lazy                     ( ByteString )
import qualified Data.ByteString.Lazy          as BL
import qualified Data.Word8                    as W8
import           Network.HTTP.Client                      ( HttpException
                                                          , Request
                                                          , httpLbs
                                                          , parseRequest
                                                          , responseBody
                                                          )
import           Text.HTML.TagSoup                        ( Tag
                                                          , isTagText
                                                          )
import           Text.HTML.TagSoup.Match                  ( tagText )

import           Model.Types                              ( ClientContext(..)
                                                          , Menu
                                                          , NoMenu(..)
                                                          )

takeNext :: [a] -> [a]
takeNext = take 1 . drop 1

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

-- | Remove text tags that only contain whitespace.
removeWhitespaceTags :: [Tag ByteString] -> [Tag ByteString]
removeWhitespaceTags =
  filter (\t -> not (isTagText t) || tagText (not . BL.all W8.isSpace) t)
