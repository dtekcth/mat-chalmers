-- |
module M.Karen where

import qualified Data.Text.Lazy as T
import           Text.HTML.TagSoup

import           M.Internal hiding (menu)
import           Util

-- | Get a restaurant that kåren has.
getKaren :: Int -> T.Text -> String -> T.Text -> IO (Maybe Restaurant)
getKaren weekday name menuUrl restUrl = do
  text <- handle' (get menuUrl)
  return $ do
    tags <- fmap parseTags text
    let days = partitions (~== "<item>") tags
    day <- safeIdx days weekday
    return $ (getRestaurant name restUrl day)

getRestaurant :: T.Text -> T.Text -> [Tag T.Text] -> Restaurant
getRestaurant name url day = Restaurant name url today
  where
    today = map getMenu parts
    parts = partitions (~== "<tr>") day

-- menu :: [Tag T.Text] -> Menu
getMenu part =
  Menu
    (T.strip . innerText . takeNext . (dropWhile (~/= "<b>")) $ s)
    (T.strip . innerText . takeNext . (dropWhile (~/= "<td>")) $ s)
  where
    s = drop 1 . dropWhile (~/= "<td>") $ part


printer :: Show a => a -> IO ()
printer = putStrLn . take 500 . show
