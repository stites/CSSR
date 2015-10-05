{-# LANGUAGE OverloadedStrings #-}
-- | = Phase I: Initialization
--
--   Phase I computes the relative frequency of all wordsin the data stream, up
-- to length @L_max + 1@. There are several ways this can be done using just a
-- single passthrough the data. In our implementation, as we scan the data, we
-- construct a parse tree which counts the occurrences of all strings whose
-- length does not exceed @L_max + 1@. There after we need only refer to the
-- parsetree, not the data. This procedure is therefore @O(N)@,and this is the
-- only sub-procedure whose time depends on N.
-- ----------------------------------------------------------------------------
module CSSR.Initialization where

import qualified Data.Map as Map
import qualified Data.Vector as Vector
import CSSR.ParseTree

-- | SET DEFAULTS =======
-- ----------------------
-- significance level
type SignificanceLevel = Float
significanceLevel :: SignificanceLevel
significanceLevel = 0

-- Same as initializing the ParseTree
-- max length of a string
maxLength = 5

-- start with a simple alphabet and dataFile
alphabet :: String
alphabet = "ab"

dataFile :: Vector.Vector Char
dataFile = Vector.fromList $ take 100 (cycle "a")
dataLength = Vector.length dataFile
dataIdx = [0..dataLength]

x = fmap (\ idx ->
      fmap (\ len ->
        Vector.slice idx len dataFile
      ) [1..maxLength]
    ) $
      take (dataLength - maxLength) dataIdx

-- | Initialize a single state containing the null suffix =======
-- ----------------------
sigma :: String
sigma = ""
l :: Integer
l = 0


