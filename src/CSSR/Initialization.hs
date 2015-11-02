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
import Data.List (nub, intercalate)
import qualified Data.Vector as V
import qualified CSSR.Parse.Tree as PT
import CSSR.CausalState.State (State, Event, Events)

-- | SET DEFAULTS =======
-- ----------------------
-- significance level
significanceLevel :: Float
significanceLevel = 0.5

-- Same as initializing the ParseTree
-- max length of a string
lMax = 2

-- start with even process alphabet and dataFiles located in:
alphabetURI :: FilePath
alphabetURI = "../test-machines/alphabets/EP_alpha"
-- alphabet :: IO [Event]
-- alphabet = readFile alphabetURI
dataURI :: FilePath
dataURI = "../test-machines/EP_stateseq"
-- dataFile :: IO [Event]
-- dataFile = readFile dataURI

-- -----------------------------------------
alphabet :: [Event]
alphabet = "AB"

dataFile :: Events
dataFile = intercalate "" [
  "BABAAAAABABABAAABABAAABABAAABAABAAAABABAAAAAABAABAAAAABAABAABABABABAA",
  "BABABAAAAAABABABAAAABABAABAAAABAABABABABABABAAABABAABABAAAABAABABABAB",
  "ABAAAAAABABAAAABAAAAAABAABAAAAABAABABAABAAABABAAAABAAAABABABABAABABAA",
  "ABABAAAAABAABAAAABAAAABAABAAABABAAAAAABABABABABABABAAAABABAAAABABABAB",
  "AAABABAABAABABAAABAABAABABABABAABABAABAAAAAAAABAABABAABABAABAABABAAAB",
  "AABAAAABAABABAABABAABAAAABABAABABAABAABABAAAABABAABABAABAABAAAABABAAA",
  "BAABABAABABABABABAAAAAAAAABABABABAABABABAAABABAABABAABABABAABABAABAAA",
  "BAABAABABABABAABABAABAABABABABAAAAAAAABABABAABAAAABAABAAABAABAABAABAA",
  "BABABAABABAAAAAABABABABABAABABABABABABAAAAAABABABABAABABABABABABABAAA",
  "ABABABAABABAABAAABABAABABABABABAAABAAAAAABAABABAABAABABAAABABAABAABAA",
  "BABABABAAABAABABABABABAAABABAAABABABABABABAABABABAAABABAABABABAABAABA",
  "BAAAABAABABAABABABABAABABAAABAABAAAABABAABABAAABAABABAABAAABAABABABAB",
  "AABABAAABAABABAAABABAAABABABAABAABABABABABABAABABABAAABAABAAAAAABABAA",
  "BAAAAAAABAAABABABAABAABABABABABAAAABAAABAABABABAAAABAABABAABABABABAAB",
  "AABABABAAABABAABAABABABAAAABABAAAA" ]
dataLength :: Int
dataLength = length dataFile
dataIdx :: [Int]
dataIdx = [0..dataLength]

sequences :: [Events]
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


