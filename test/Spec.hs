import Test.Hspec
import Test.QuickCheck
import Control.Exception (evaluate)
import CSSR.Initialization

main :: IO ()
main = hspec $ do
  describe "Prelude.head" $ do
    it "returns the first element of a list" $ do
      head [23 ..] `shouldBe` (23 :: Int)

    it "returns the first element of an *arbitrary* list" $
      property $ \x xs -> head (x:xs) == (x :: Int)

    it "throws an exception if used with an empty list" $ do
      evaluate (head []) `shouldThrow` anyException

  describe "CSSR with Even Process" $ do
    describe "\"AB\" alphabet with lMax=1" $ do
      it "should return two states with 'A' and 'B'" $
        lMax `shouldBe` (1 :: Int)
