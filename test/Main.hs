import qualified Data.ByteString.Lazy          as BL
import qualified Data.ByteString.Lazy.Char8    as BL8
import qualified Data.Text.Lazy                as T
import           Data.Thyme.Time.Core                     ( fromGregorian )
import           Model.Karen                              ( parse )
import           Model.Types                              ( Menu(..)
                                                          , NoMenu
                                                          )
import           Model.Wijkanders                         ( getWijkanders
                                                          , hasDate
                                                          )
import           Test.Hspec                               ( describe
                                                          , hspec
                                                          , it
                                                          )
import           Test.HUnit                               ( (@?=)
                                                          , assertFailure
                                                          )

testFun :: [Menu] -> Either NoMenu [Menu] -> IO ()
testFun expected = either
  (assertFailure . mappend "This is not expected, input was:\n" . show)
  (@?= expected)

main :: IO ()
main = hspec $ do
  describe "hasDate"
    $ it "parses a legit date" (hasDate (BL8.pack "2/1") @?= Just (1, 2))
  describe "The Karen Express" $ it
    "parses a blob of JSON without error"
    ( testFun
        [ Menu (T.pack "Express")
               (T.pack "Kycklinggryta, persilja, senap & kokt potatis")
        , Menu (T.pack "Express - Vegan")
               (T.pack "Vegetariska biffar, persiljekr\228m & potatismos")
        ]

    $ parse
        "Swedish"
        (BL8.pack
          "{\"data\":{\"dishOccurrencesByTimeRange\":[{\"displayNames\":[{\"name\":\"Kycklinggryta, persilja, senap & kokt potatis\",\"categoryName\":\"Swedish\"},{\"name\":\"Chicken stew, parsley, mustard & boiled potatoes\",\"categoryName\":\"English\"}],\"startDate\":\"9/9/2019 12:00:00 AM\",\"dishType\":{\"name\":\"Express\"},\"dish\":{\"name\":\"Kycklinggryta, aubergin, zucchini, tomat & rostad potatis\"}},{\"displayNames\":[{\"name\":\"Vegetariska biffar, persiljekr\195\164m & potatismos\",\"categoryName\":\"Swedish\"},{\"name\":\"Vegetarian patties, parsley cream & mashed potatoes\",\"categoryName\":\"English\"}],\"startDate\":\"9/9/2019 12:00:00 AM\",\"dishType\":{\"name\":\"Express - Vegan\"},\"dish\":{\"name\":\"Vegetariska biffar, majonn\195\164s & potatispur\195\169\"}}]}}\n"
        )
    )

  describe "The Wijkander's"
    $ it "Parses two blobs of HTML correctly on fridays"
    $ do
        s1 <- BL.readFile "test/190517 wijkanders.html"
        testFun
          [ Menu
            (T.pack "Fisk")
            (T.pack
              "Havets Wallenbergare, kallpressad rapsolja, ärtor, dill & potatismos"
            )
          , Menu
            (T.pack "Kött")
            (T.pack
              "Helstekt kotlettred, potatisgratäng, skysås & örtbakad tomat"
            )
          ]
          (getWijkanders (fromGregorian 2019 05 17) s1)
        s2 <- BL.readFile "test/190913 wijkanders.html"
        testFun
          [ Menu
            (T.pack "Vegetarisk ")
            (T.pack
              "Pasta, svamp, grädde, citron, grana padano & rotfruktschips"
            )
          , Menu
            (T.pack "Fisk")
            (T.pack "Torskbiff, brynt smör, hackat ägg, dill- & potatismos")
          , Menu
            (T.pack "Kött")
            (T.pack
              "Helstekt kotlettrad, rostad potatis, svampsås & inlagd gurka"
            )
          ]
          (getWijkanders (fromGregorian 2019 09 13) s2)
