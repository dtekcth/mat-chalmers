-- |
module M.Karen where

import qualified Data.Text.Lazy as T
import           Data.Thyme
import           System.Locale (defaultTimeLocale)
import           Text.HTML.TagSoup

import           M.Internal hiding (menu)

-- | Get a restaurant that kåren has.
getKaren :: LocalTime -> T.Text -> String -> IO (Maybe Restaurant)
getKaren date name format = handle' (fmap (getRestaurant name) (get url))
  where
    url = (formatTime defaultTimeLocale format) date

getRestaurant :: T.Text -> T.Text -> Restaurant
getRestaurant name text = Restaurant name today
  where
    today = map menu sects
    sects = partitions (~== "<item>") tags :: [[Tag T.Text]]
    tags = parseTags text :: [Tag T.Text]

menu :: [Tag T.Text] -> Menu
menu section =
    Menu
        (innerText . takeNext . (dropWhile (~/= "<title>")) $ section)
        (T.takeWhile (/= '@') .
         T.strip . innerText . takeNext . (dropWhile (~/= "<description>")) $
         section)
  where
    takeNext = take 1 . drop 1
