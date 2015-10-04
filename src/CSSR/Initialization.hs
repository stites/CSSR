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
module Initialization where

import qualified Data.Map as Map

-- significance level
type SignificanceLevel = Integer
significanceLevel :: SignificanceLevel
significanceLevel = 1

-- max length of a string
maxLength :: Integer
maxLength = 3

-- TreeNode is a state representation of the input file
data TreeNode a = Empty | Leaf a | Node (TreeNode a) (TreeNode a)

-- parseTree is a hashmap of the state
parseTree :: Map.Map String (TreeNode a)
parseTree = Map.empty



