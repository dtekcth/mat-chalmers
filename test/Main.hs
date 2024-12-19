{-# LANGUAGE LambdaCase #-}
import qualified Data.ByteString.Lazy          as BL
import qualified Data.ByteString.Lazy.Char8    as BL8
import qualified Data.Text.Lazy                as T
import           Data.Aeson                               ( decode )
import           Data.Maybe                               ( fromJust )
import           Data.Thyme.Time.Core                     ( fromGregorian )
import           Model.Karen                              ( parse )
import qualified Model.Linsen                  as L       ( parse )
import           Model.Types                              ( Menu(..)
                                                          , NoMenu( NoLunch, NMParseError )
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

testFun :: Either NoMenu [Menu] -> Either NoMenu [Menu] -> IO ()
testFun = \case
    Right e ->
      either
      (assertFailure . mappend "This is not expected, input was:\n" . show)
      (@?= e)
    Left (NMParseError e _) ->
      either
      ((@?= e) . \(NMParseError e1 _) -> e1)
      (assertFailure . mappend "This is not expected, input was:\n" . show)
    Left e ->
      either
      (@?= e)
      (assertFailure . mappend "This is not expected, input was:\n" . show)

main :: IO ()
main = hspec $ do
  describe "hasDate"
    $ it "parses a legit date" (hasDate (BL8.pack "2/1") @?= Just (1, 2))
  describe "The Karen Express" $ it
    "parses a blob of JSON without error"
    ( testFun
        (Right [ Menu
            (T.pack "Street food")
            (T.pack "Chicken africana, banan, mango raja, ris")
        , Menu
            (T.pack "Greens")
            (T.pack "Indisklinsgryta, zucchini, aubergin, ingef\228ra, koriander, ris")
        , Menu
            (T.pack "Nordic")
            (T.pack "F\228rskost bakad sej, vitvinss\229s, broccoli, potatis")
        ])
    $ parse
        "Swedish"
        (fromJust . decode $ BL8.pack
          "{\"data\":{\"dishOccurrencesByTimeRange\":[{\"displayNames\":[{\"name\":\"Chicken africana, banan, mango raja, ris\",\"categoryName\":\"Swedish\"},{\"name\":\"Chicken africana, banana, mango raja, rice\",\"categoryName\":\"English\"}],\"startDate\":\"09/25/2023 00:00:00\",\"dishType\":{\"name\":\"Street food\"},\"dish\":{\"name\":\"Kyckling, het paprikas\195\165s & ris\"}},{\"displayNames\":[{\"name\":\"Indian linseed stew, zucchini, aubergine, ginger, coriander\",\"categoryName\":\"English\"},{\"name\":\"Indisklinsgryta, zucchini, aubergin, ingef\195\164ra, koriander, ris\",\"categoryName\":\"Swedish\"}],\"startDate\":\"09/25/2023 00:00:00\",\"dishType\":{\"name\":\"Greens\"},\"dish\":{\"name\":\"Vegan, pasta, linsbolognese\"}},{\"displayNames\":[{\"name\":\"F\195\164rskost bakad sej, vitvinss\195\165s, broccoli, potatis\",\"categoryName\":\"Swedish\"},{\"name\":\"Cream cheese baked saithe, whitewine sauce, broccoli, potatoes\",\"categoryName\":\"English\"}],\"startDate\":\"09/25/2023 00:00:00\",\"dishType\":{\"name\":\"Nordic\"},\"dish\":{\"name\":\"Bakad fisk, vitvinss\195\165s, potatispur\195\169\"}}]}}\n"
        )
    )

  describe "The Karen Express" $ it
    "parses a blob of JSON without error, but it has an dish without dishType"
    ( testFun
        (Right [ Menu
            (T.pack "Unknown menu")
            (T.pack "Fläskfilé, svampsås & rostad klyftpotatis")
        , Menu
            (T.pack "Greens")
            (T.pack "Bönburgare, syrad vitkål- morot, vitlöksdressing & rostad potatis")
        , Menu
            (T.pack "Street food")
            (T.pack "Färskost bakad fisk, vitvinssås, broccoli, potatis")
        , Menu
            (T.pack "Nordic")
            (T.pack "Köttbullar, gräddsås, potatispuré, rårörda lingon, pressgurka")
        ])
    $ parse
        "Swedish"
        (fromJust . decode $ BL8.pack "{\"data\":{\"dishOccurrencesByTimeRange\":[{\"dish\":{\"name\":\"Fl\195\164skfil\195\169, svamps\195\165s & rostad klyftpotatis\"},\"dishType\":null,\"displayNames\":[{\"categoryName\":\"Swedish\",\"name\":\"Fl\195\164skfil\195\169, svamps\195\165s & rostad klyftpotatis\"}],\"startDate\":\"08/14/2024 00:00:00\"},{\"dish\":{\"name\":\"B\195\182nbiff, rostad matvetesallad, purjol\195\182k, citroncr\195\168me\\n\"},\"dishType\":{\"name\":\"Greens\"},\"displayNames\":[{\"categoryName\":\"Swedish\",\"name\":\"B\195\182nburgare, syrad vitk\195\165l- morot, vitl\195\182ksdressing & rostad potatis\"},{\"categoryName\":\"English\",\"name\":\"Beanburger, pickled cabbage- carrot, garlic dressin & roasted potatoe\"}],\"startDate\":\"08/14/2024 00:00:00\"},{\"dish\":{\"name\":\"Bakad fisk, vitvinss\195\165s, potatispur\195\169\"},\"dishType\":{\"name\":\"Street food\"},\"displayNames\":[{\"categoryName\":\"Swedish\",\"name\":\"F\195\164rskost bakad fisk, vitvinss\195\165s, broccoli, potatis\"},{\"categoryName\":\"English\",\"name\":\"Cream cheese baked fish, whitewine sauce, broccoli, potatoes\"}],\"startDate\":\"08/14/2024 00:00:00\"},{\"dish\":{\"name\":\"K\195\182ttbullar, gr\195\164dds\195\165s, potatispur\195\169, lingon\"},\"dishType\":{\"name\":\"Nordic\"},\"displayNames\":[{\"categoryName\":\"English\",\"name\":\"Meat balls, cream sauce, mashed potato, lingonberries, pickled cucumber\"},{\"categoryName\":\"Swedish\",\"name\":\"K\195\182ttbullar, gr\195\164dds\195\165s, potatispur\195\169, r\195\165r\195\182rda lingon, pressgurka\"}],\"startDate\":\"08/14/2024 00:00:00\"}]}}"
        )
    )

  describe "Cafe Linsen" $ it
    "parses a blob of JSON without error"
    (do
      s1 <- BL.readFile "test/linsen1.json"
      testFun
        (Right [ Menu
            (T.pack "Raggmunk.")
            (T.pack "Stekt Fläsk, Lingon, Persilja.")
        , Menu
            (T.pack "Pocherad Dagens Fångst.")
            (T.pack "Sandefjordsås, Brocoli, Brynt Lime, Gräslök Stekt Potatis.")
        , Menu
            (T.pack "Dagens Pasta.")
            (T.pack "Tortelioni, Tryffelsås, Baby Spenat, Grana Padano, Roccola.")
        ]) (L.parse
              (fromGregorian 2024 09 24)
              (fromJust $ decode s1))
    )

  describe "Cafe Linsen" $ it
    "parses a blob of JSON without error, that has no lunch"
    (do
      s2 <- BL.readFile "test/linsen2.json" -- Test that has no lunch
      testFun (Left NoLunch)
        (L.parse
              (fromGregorian 2024 06 06)
              (fromJust $ decode s2))
    )


  describe "Cafe Linsen" $ it
    "parses a blob of JSON without error, that has the wrong week"
    (do
      s3 <- BL.readFile "test/linsen3.json"
      testFun (Left (NMParseError "Error in $: Unable to parse day" undefined))
        (L.parse
              (fromGregorian 2024 06 26)
              (fromJust $ decode s3))
    )

  describe "The Wijkander's"
    $ it "Parses two blobs of HTML correctly on fridays"
    $ do
        s1 <- BL.readFile "test/190517 wijkanders.html"
        testFun
          (Right [ Menu
            (T.pack "Fisk")
            (T.pack
              "Havets Wallenbergare, kallpressad rapsolja, ärtor, dill & potatismos"
            )
          , Menu
            (T.pack "Kött")
            (T.pack
              "Helstekt kotlettred, potatisgratäng, skysås & örtbakad tomat"
            )
          ])
          (getWijkanders (fromGregorian 2019 05 17) s1)
        s2 <- BL.readFile "test/190913 wijkanders.html"
        testFun
          (Right [ Menu
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
          ])
          (getWijkanders (fromGregorian 2019 09 13) s2)

  describe "The Wijkander's"
    $ it "Parses blob of HTML that fails on weekends"
    $ do
        s1 <- BL.readFile "test/241016 wijkanders.html"
        testFun
          (Left NoLunch)
          (getWijkanders (fromGregorian 2024 10 19) s1)

  describe "The Wijkander's"
    $ it "Parses blob of HTML that fails on a thursday with faulty parsing"
    $ do
        s1 <- BL.readFile "test/241016 wijkanders.html"
        testFun
          (Left (NMParseError "Wijkanders failed" undefined))
          (getWijkanders (fromGregorian 2024 10 17) s1)
