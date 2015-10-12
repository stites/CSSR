-------------------------------------------------------------------------------
-- | = Phase I: Initialization
--
--   Phase I computes the relative frequency of all wordsin the data stream, up
-- to length @L_max + 1@. There are several ways this can be done using just a
-- single passthrough the data. In our implementation, as we scan the data, we
-- construct a parse tree which counts the occurrences of all strings whose
-- length does not exceed @L_max + 1@. There after we need only refer to the
-- parsetree, not the data. This procedure is therefore @O(N)@,and this is the
-- only sub-procedure whose time depends on N.
-------------------------------------------------------------------------------
module CSSR.Initialization where

-- TODO: benchmark Vectorsbefore swapping the lists with them
import Data.List (nub)
import qualified Data.Vector as V
import qualified CSSR.Parse.Tree as PT
import CSSR.CausalState.History (Moment)
import CSSR.CausalState.State (State)

-- | SET DEFAULTS =======
-- ----------------------
-- significance level
significanceLevel :: Float
significanceLevel = 0.05

-- Same as initializing the ParseTree
-- max length of a string
lMax = 5

-- start with a simple alphabet and dataFile
alphabet :: [Moment]
alphabet = "ab"

dataFile :: [Moment]
dataFile =  take 100 (cycle "a")
dataLength = length dataFile
dataIdx = [0..dataLength]

sequences = nub $ concat $ findSequences dataFile
  where
     findSequences dataFile = let
            getWindowFromList start len dataFile = V.toList $ V.slice start len ( V.fromList dataFile )

         in fmap (\ idx -> fmap (\ len -> getWindowFromList idx len dataFile) [1..lMax]) $
            take (dataLength - lMax) dataIdx

parseTree :: PT.ParseTree
parseTree = PT.Root $ foldl PT.build [] sequences

-- | Initialize a single state containing the null suffix =======
-- ----------------------
sigma :: [State]
sigma = []
l :: Int
l = 0


