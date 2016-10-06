-- |
module M.Karen where

import qualified Data.Text.Lazy as T
import           Text.HTML.TagSoup

import           M.Internal hiding (menu)
import           Util

-- | Get a restaurant that kåren has.
getKaren :: Int -> T.Text -> String -> IO (Maybe Restaurant)
getKaren weekday name url = do
  text <- handle' (get url)
  return $ do
    tags <- fmap parseTags text
    let days = partitions (~== "<item>") tags
    day <- safeIdx days weekday
    return $ (getRestaurant name day)

getRestaurant :: T.Text -> [Tag T.Text] -> Restaurant
getRestaurant name day = Restaurant name today
  where
    today = map getMenu parts
    parts = partitions (~== "<tr>") day
    -- sects = partitions (~== "<item>") tags :: [[Tag T.Text]]

-- menu :: [Tag T.Text] -> Menu
getMenu part =
  Menu
    (T.strip . innerText . takeNext . (dropWhile (~/= "<b>")) $ s)
    (T.strip . innerText . takeNext . (dropWhile (~/= "<td>")) $ s)
  where
    s = drop 1 . dropWhile (~/= "<td>") $ part


printer :: Show a => a -> IO ()
printer = putStrLn . take 500 . show
