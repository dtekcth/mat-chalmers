{-# LANGUAGE FlexibleContexts, LambdaCase, MultiWayIf, OverloadedStrings #-}
{-# LANGUAGE PatternSynonyms #-}

module Model.Linsen
  (
    parse
  , fetchAndCreateLinsen
  )
where

import           Control.Monad                            ( (>=>)
                                                          , (<=<)
                                                          , zipWithM
                                                          , filterM
                                                          , ap )
import           Data.Aeson                               ( (.:)
                                                          , (.:?)
                                                          , (.!=)
                                                          , withObject
                                                          , Object
                                                          , Value
                                                          )
import           Data.Aeson.Types                         ( Parser
                                                          , parseEither )
import           Data.Bifunctor                           ( first )
import qualified Data.ByteString.Lazy.Char8    as BL8
import           Data.Char                                ( isSpace )
import           Data.Functor                             ( (<&>) )
import           Data.List.Extra                          ( (!?) )
import           Data.Monoid                              ( Alt(..) )
import           Data.Text.Lazy                           ( Text
                                                          , all
                                                          , replace
                                                          , strip )
import           Data.Thyme                               ( parseTime )
import           Data.Thyme.Calendar                      ( Day )
import           Data.Thyme.Calendar.WeekDate             ( weekDate
                                                          , _wdDay )
import           Effectful                                ( (:>)
                                                          , Eff
                                                          )
import           Effectful.Exception                      ( handle )
import           Lens.Micro.Platform                      ( (^.) )
import           Effectful.Wreq                           ( Wreq
                                                          , asValue
                                                          , get
                                                          , responseBody )
import           Model.Types                              ( NoMenu(..)
                                                          , Menu(..)
                                                          , Restaurant ( Restaurant ) )
import           Prelude                       hiding     ( all )
import           Util                                     ( menusToEitherNoLunch
                                                          , networkExceptionHandler
                                                          , swedishTimeLocale
                                                          , (^.^)
                                                          )


pattern MeatDish, FishDish, VegDish :: Integer
pattern MeatDish = 2
pattern FishDish = 6
pattern VegDish = 10

fetch :: (Wreq :> es) => Eff es Value  -- ^ A JSON response or horrible crash
fetch =
  get "https://cafe-linsen.se/api/menu" >>= asValue >>= (^.^ responseBody)

parse
  :: Day                  -- ^ Day to parse
  -> Value                -- ^ JSON result from `fetch`
  -> Either NoMenu [Menu] -- ^ Either list of parsed `Menu`s or `NoMenu` error
parse day =
    failWithNoMenu
      (parseEither
        (let index = 6 - day ^. weekDate . _wdDay in
        if index `notElem` [1..5] then
            pure . const []
        else
          withObject "Parse meals"
          $   (.: "docs")
          >=> (\case
                Nothing -> fail "Failed to index into days"
                Just v -> pure v) . (!? index)
          >=> (.: "richText")
          >=> (.: "root")
          >=> (.: "children")
          >=> (\case
                  [] -> fail "No items in day"
                  [x] -> (x .: "children")
                  xs -> pure xs)
          >=> filterM ((.: "children") >=> \case
                            []    -> pure False
                            (v:_) -> (v .:? "text" .!= " ") <&> not . all isSpace)
          >=> (\v' ->
                 (case v' !? 1 of
                   Nothing -> fail "failed to index into food"
                   Just v -> pure v) >>=
                  ((.: "children")
                  >=> (\case
                          []    -> fail "Failed to index into richtext"
                          v -> pure $ mconcat v)
                  >=> (.: "text")
                  >=> filterM (pure . not . isSpace)
                  >=> \s ->
                    let separators = ["-", "/", "\8211\&", ""]
                          -- Order here matters, since 01-09-2025 could be parsed as 01 09 -2025, which is a valid date
                        parsedDate = getAlt . foldMap (Alt . flip (parseTime swedishTimeLocale) s) $
                          (\a b c -> a <> "%d" <> b <> "%m" <> c <> "%Y") <$>
                            ("" : (("%A" <> ) <$> separators)) <*>
                            separators <*>
                            separators
                        sameDay = pure day == parsedDate
                     in if | sameDay && length v' >= 9 -> pure v'
                           | sameDay                   -> pure mempty
                           | otherwise                 -> fail "Unable to parse day"))
          >=> menuParser
        )
      )
    >=> menusToEitherNoLunch
 where
  failWithNoMenu :: Show a => (a -> Either String b) -> a -> Either NoMenu b
  failWithNoMenu action x =
    first (\msg -> NMParseError msg . BL8.pack . show $ x) (action x)

  menuParser :: [Object] -> Parser [Menu]
  menuParser = pure . (zip [0 :: Integer ..] >=> \case
                          (MeatDish, vs) -> [vs]
                          (FishDish, vs) -> [vs]
                          (VegDish , vs) -> [vs]
                          _       -> [])
               <=< ap (zipWithM sumFood) tail

  sumFood :: Object -> Object -> Parser Menu
  sumFood a b = Menu <$> getFood a <*> getFood b

  getFood :: Object -> Parser Text
  getFood = (.: "children") >=> \case
                  [] -> pure mempty
                  vs -> strip . replace "/ " ", " . mconcat
                        <$> mapM (.: "text") vs

fetchAndCreateLinsen
  :: (Wreq :> es)
  => Day          -- ^ Day
  -> Eff es Restaurant -- ^ Fetched Restaurant
fetchAndCreateLinsen day =
  Restaurant
      "Café Linsen"
      "https://cafe-linsen.se/#menu"
    <$> handle networkExceptionHandler
          (parse day <$> fetch)
