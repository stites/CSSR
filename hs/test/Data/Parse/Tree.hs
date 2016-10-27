module Data.Parse.Tree where

import CSSR.TestPrelude

main :: IO ()
main = hspec spec

spec :: Spec
spec =
  describe "a parse tree" $ do
    describe "when we encounter the history 110" $ do
      it "makes 0 the child of the root node" $ do

      it "makes 1 the child of  0  (tree has 10)" $ do
      it "makes 1 the child of 10 (tree has 110)" $ do




