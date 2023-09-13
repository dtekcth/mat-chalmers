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
