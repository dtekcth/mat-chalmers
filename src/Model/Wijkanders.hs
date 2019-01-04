module Model.Wijkanders
  ( getWijkanders
  )
where

import           Data.Char
import           Data.Maybe                               ( fromMaybe
                                                          , mapMaybe
                                                          )
import           Safe                                     ( atMay
                                                          , headMay
                                                          )
import qualified Data.Text.Lazy                as T
import           GHC.Exts
import           Text.HTML.TagSoup

import           Model.Types                       hiding ( menu
                                                          , date
                                                          )

getWijkanders :: Int -> Maybe T.Text -> Restaurant
getWijkanders weekday text =
  let ts = parseTags <$> text
  in  mkRestaurant . fromMaybe (Left (SomethingWrong text)) $ do
        tags <- ts
        dayt <- getDay weekday tags
        mst  <- getMenus dayt
        let ms = mapMaybe mkMenu mst
        return $ case ms of
          [] -> Left NoLunch
          _  -> Right ms

getDay :: Int -> [Tag T.Text] -> Maybe [Tag T.Text]
getDay weekday tags = do
  post <- headMay $ partitions (~== "<div class='post-content'>") tags
  let ps   = partitions (~== "<p>") post
  let days = drop 5 ps
  atMay days weekday

getMenus :: [Tag T.Text] -> Maybe [[Tag T.Text]]
getMenus day = do
  let ms = drop 1 $ partitions (~== "<strong>") day
  return ms

mkMenu :: [Tag T.Text] -> Maybe Menu
mkMenu m = do
  what <- atMay m 1
  food <- atMay m 3
  let lunch = T.toTitle . T.takeWhile isAlpha . text $ [what]
  if T.null lunch
    then Nothing
    else return $ Menu lunch (T.dropWhile (not . isAlpha) . text $ [food])

text = T.strip . innerText

mkRestaurant = Restaurant
  (fromString "Wijkanders")
  (fromString "http://www.wijkanders.se/restaurangen/")