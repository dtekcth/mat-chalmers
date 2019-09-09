import qualified Data.ByteString.Lazy.Char8    as BL8
import qualified Data.Text.Lazy                as T
import           Model.Karen                              ( parse )
import           Model.Types                              ( Menu(..) )
import           Test.Hspec                               ( describe
                                                          , hspec
                                                          , it
                                                          )
import           Test.HUnit                               ( (@?=)
                                                          , assertFailure
                                                          )

main :: IO ()
main = hspec $ describe "The Karen Express" $ it
  "parses a blob of JSON without error"
  ( either
      (assertFailure . mappend "This is not expected, input was:\n" . show)
      (@?= [ Menu (T.pack "Express")
                  (T.pack "Kycklinggryta, persilja, senap & kokt potatis")
           , Menu (T.pack "Express - Vegan")
                  (T.pack "Vegetariska biffar, persiljekr\228m & potatismos")
           ]
      )
  $ parse
      "Swedish"
      (BL8.pack
        "{\"data\":{\"dishOccurrencesByTimeRange\":[{\"displayNames\":[{\"name\":\"Kycklinggryta, persilja, senap & kokt potatis\",\"categoryName\":\"Swedish\"},{\"name\":\"Chicken stew, parsley, mustard & boiled potatoes\",\"categoryName\":\"English\"}],\"startDate\":\"9/9/2019 12:00:00 AM\",\"dishType\":{\"name\":\"Express\"},\"dish\":{\"name\":\"Kycklinggryta, aubergin, zucchini, tomat & rostad potatis\"}},{\"displayNames\":[{\"name\":\"Vegetariska biffar, persiljekr\195\164m & potatismos\",\"categoryName\":\"Swedish\"},{\"name\":\"Vegetarian patties, parsley cream & mashed potatoes\",\"categoryName\":\"English\"}],\"startDate\":\"9/9/2019 12:00:00 AM\",\"dishType\":{\"name\":\"Express - Vegan\"},\"dish\":{\"name\":\"Vegetariska biffar, majonn\195\164s & potatispur\195\169\"}}]}}\n"
      )
  )
