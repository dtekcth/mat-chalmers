import qualified Data.ByteString.Lazy          as BL
import qualified Data.ByteString.Lazy.Char8    as BL8
import qualified Data.Text.Lazy                as T
import           Data.Aeson                               ( decode )
import           Data.Maybe                               ( fromJust )
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
        [ Menu
            (T.pack "Street food")
            (T.pack "Chicken africana, banan, mango raja, ris")
        , Menu
            (T.pack "Greens")
            (T.pack "Indisklinsgryta, zucchini, aubergin, ingef\228ra, koriander, ris")
        , Menu
            (T.pack "Nordic")
            (T.pack "F\228rskost bakad sej, vitvinss\229s, broccoli, potatis")
        ]
    $ parse
        "Swedish"
        (fromJust . decode $ BL8.pack
          "{\"data\":{\"dishOccurrencesByTimeRange\":[{\"displayNames\":[{\"name\":\"Chicken africana, banan, mango raja, ris\",\"categoryName\":\"Swedish\"},{\"name\":\"Chicken africana, banana, mango raja, rice\",\"categoryName\":\"English\"}],\"startDate\":\"09/25/2023 00:00:00\",\"dishType\":{\"name\":\"Street food\"},\"dish\":{\"name\":\"Kyckling, het paprikas\195\165s & ris\"}},{\"displayNames\":[{\"name\":\"Indian linseed stew, zucchini, aubergine, ginger, coriander\",\"categoryName\":\"English\"},{\"name\":\"Indisklinsgryta, zucchini, aubergin, ingef\195\164ra, koriander, ris\",\"categoryName\":\"Swedish\"}],\"startDate\":\"09/25/2023 00:00:00\",\"dishType\":{\"name\":\"Greens\"},\"dish\":{\"name\":\"Vegan, pasta, linsbolognese\"}},{\"displayNames\":[{\"name\":\"F\195\164rskost bakad sej, vitvinss\195\165s, broccoli, potatis\",\"categoryName\":\"Swedish\"},{\"name\":\"Cream cheese baked saithe, whitewine sauce, broccoli, potatoes\",\"categoryName\":\"English\"}],\"startDate\":\"09/25/2023 00:00:00\",\"dishType\":{\"name\":\"Nordic\"},\"dish\":{\"name\":\"Bakad fisk, vitvinss\195\165s, potatispur\195\169\"}}]}}\n"
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
