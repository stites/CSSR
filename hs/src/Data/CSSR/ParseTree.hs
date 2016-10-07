{-# LANGUAGE OverloadedLists #-}
module Data.CSSR.ParseTree where

---------------------------------------------------------------------------------
-- We encounter the history say "110"
-- We go to the parse tree at the root
-- We take the 0 child of the root
-- We then take the 1 child of 0 (= 10)
-- We then take the 1 child of 10 (=110)
---------------------------------------------------------------------------------
data ParseTree = ParseTree
  { alphabet :: Alphabet
  , root :: Tree -- testing stuffffff
  , maxLength :: Int
  , dataSize :: Double
  } deriving (Show, Eq)

takeLengthOf :: [a] -> [b] -> [b]
takeLengthOf = zipWith (flip const)

windows :: Int -> [a] -> [[a]]
windows n xs = takeLengthOf (drop (n-1) xs) (windows' n xs)

loadData :: Foldable f => ParseTree -> f Char -> Int -> ParseTree
loadData tree chars n = pTree
  where
    banned :: HashSet Char
    banned = "\r\n"

    filtered :: [Char]
    filtered = filter (\x -> not (x `elem` banned)) chars

    pTree = ParseTree
      { alphabet = Alphabet
      , root = Nothing
      , maxLength = n
      , dataSize = length filtered
      }

    concat
      ( map (\xs -> cleanInsert tree (map fst xs) (map snd xs)) (windows n $ zip filtered [1..]))
      ( map {-tree.getDepth(depth).foreach(_.calcNextStepProbabilities(tree))-} $ last n (windows n $ zip filtered [1..]))
    removeChildren tree n


  getCurrent = last
  getPrior = init
  type CurrentHistory = (Char, Int)
  type OlderHistory = (Iterable[Char], Iterable[Int])
  protected def splitHistoryClean (observed: Iterable[Char], idx:Iterable[Int]): (CurrentHistory , OlderHistory) = {
    ((getCurrent(observed), getCurrent(idx)), (getPrior(observed), getPrior(idx)))
  }

  type GetCurrent[A >: AnyVal] = (List[A])=>A
  type GetPrior[A >: AnyVal] = (List[A])=>List[A]

  cleanInsert :: Foldable f
              => ParseTree
              -> f Char
              -> f Int
              -> ParseTree
  cleanInsert tree observed idx = go observed (root tree) tree observed idx
    where
      go :: Foldable f
         -> ParseTree
         -> f Char
         -> ParseLeaf
         -> ParseTree
         -> [Char]
         -> f Int
         -> ParseTree
      go memo      [] active tree fullHistory idx = memo
      go memo history active tree fullHistory idx =
        let
          ((current, cIdx), (older, oIdx)) = splitHistoryClean history idx
          maybeNext = next active current :: Maybe ParseLeaf
          next' = maybe (addChild tree active current) maybeNext
        in
          undefined

--           if (maybeNext.nonEmpty) next.obsCount += 1
--              next.addLocation(cIdx)
--              go(older, next, tree, fullHistory, oIdx)

