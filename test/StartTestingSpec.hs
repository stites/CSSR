module StartTestingSpec(spec) where

import Test.Hspec
import Test.Hspec.QuickCheck

spec :: Spec
spec = do
  describe "initial described block" $ do
    prop "property" $ \x y ->
      add x y == add y x

add :: Integer -> Integer -> Integer
add x y = x + y

