{-# LANGUAGE OverloadedLists #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE TemplateHaskell #-}

module Data.Parse.Tree
  -- ( PLeaf
  -- ) where
  where

import qualified Data.HashSet as HS
import qualified Data.HashMap.Strict as HM
import qualified Data.Vector as V

import CSSR.Prelude
import CSSR.TypeAliases

data ParseTree = ParseTree
  { maxLength :: Int
  , dataSize :: Int
  , root :: PLeaf
  } deriving (Show, Eq)

instance Monoid ParseTree where
  mempty = ParseTree 0 0 mkRoot
  (ParseTree len0 size0 root0) `mappend` (ParseTree len1 size1 root1)
  -- FIXME: I know! at least this will work until I figure out the right way to
  -- do this (with symbols?)
    | len0 /= len1 = error "can't append ParseTrees of unequal max length"
    | otherwise = ParseTree len0 (size0 + size1) (union root0 root1)

data PLeaf = PLeaf
  { _obs :: Vector Event
  , _count :: Integer
  , _parent :: Parent
  , _children :: Children
  , _locations :: Locations
  } deriving (Show, Eq)

type Event = Char
type Children = HashMap Event PLeaf
type Parent = Maybe PLeaf
type Alphabet = [Event]

union (PLeaf o0 c0 p0 cs0 ls0) (PLeaf o1 c1 p1 cs1 ls1)
  | o0 /= o1 || p0 /= p1 = error "can't union Parse Leaves of different observations"
  | otherwise = PLeaf o0 (c0+c1) p0 (unionChilds cs0 cs1) (unionLocs ls0 ls1)
  where
    unionChilds :: Children -> Children -> Children
    unionChilds = HM.unionWith union

    unionLocs :: Locations -> Locations -> Locations
    unionLocs = HM.unionWith (+)

current :: Vector Event -> Event
current = V.last

prior :: Vector Event -> Vector Event
prior = V.init

mkRoot :: PLeaf
mkRoot = PLeaf [] 0 Nothing mempty mempty

mkLeaf :: Vector Event -> PLeaf -> PLeaf
mkLeaf obs p = PLeaf obs 1 (Just p) mempty mempty

findChild :: PLeaf -> Event -> Maybe PLeaf
findChild lf c = HM.lookup c (_children lf)

navigate :: PLeaf -> Vector Event -> Maybe PLeaf
navigate active      [] = Just active
navigate active history =
  case findChild active (current history) of
    Just next -> navigate next (prior history)
    Nothing   -> Nothing

navigateTree :: ParseTree -> Vector Event -> Maybe PLeaf
navigateTree ParseTree{..} = navigate root

makeLenses ''PLeaf

addLocation :: PLeaf -> Idx -> PLeaf
addLocation lf i = over locations (increment i) lf
  where
    increment :: Idx -> Locations -> Locations
    increment i locs = HM.insertWith (+) i 1 locs

---------------------------------------------------------------------------------
-- We encounter the history say "110"
-- We go to the parse tree at the root
-- We take the 0 child of the root
-- We then take the 1 child of 0 (= 10)
-- We then take the 1 child of 10 (=110)
---------------------------------------------------------------------------------

takeLengthOf :: [a] -> [b] -> [b]
takeLengthOf = zipWith (flip const)

--windows :: Int -> [a] -> [[a]]
--windows n xs = takeLengthOf (drop (n-1) xs) (windows' n xs)

type DataFileContents = Vector Event

isInvalid :: Event -> Bool
isInvalid = flip HS.member ['\r', '\n']

isValid :: Event -> Bool
isValid = not . isInvalid

takeEvents :: Int -> Vector Event -> Vector Event
takeEvents n = V.take n . V.filter isValid

streamToWindows :: Int -> Vector Event -> Vector (Vector Event)
streamToWindows n es = V.imap mapper es
  where
    mapper :: Int -> Event -> Vector Event
    mapper i _ = V.slice i n es

buildBranch :: Vector Event -> PLeaf -> PLeaf -> PLeaf
buildBranch [] active@PLeaf{..} topRef = topRef
buildBranch es active@PLeaf{..} topRef = undefined
  where
    e :: Event
    e = current es

    newLeaf :: PLeaf
    newLeaf = mkLeaf (V.snoc _obs e) active

    child :: PLeaf
    child = maybe newLeaf (over count (+1)) $ findChild active e



buildTree :: DataFileContents -> (Int, PLeaf)
buildTree chars = go chars root (0, root)
  where
    root :: PLeaf
    root = mkRoot

    go :: DataFileContents -> PLeaf -> (Int, PLeaf) -> (Int, PLeaf)
    go alldata active (nFiltered, root) = undefined

loadData :: DataFileContents -> Int -> ParseTree
loadData chars n = ParseTree
  { maxLength = n
  , dataSize = length chars - undefined -- nFiltered
  , root = undefined -- root
  }

-- cleanInsert :: Foldable f
--             => PLeaf
--             -> f Event
--             -> f Int
--             -> PLeaf
-- cleanInsert tree observed idx = go observed (root tree) tree observed idx
--   where
--     go :: Foldable f
--        -> PLeaf
--        -> f Event
--        -> ParseLeaf
--        -> PLeaf
--        -> [Event]
--        -> f Int
--        -> PLeaf
--     go memo      [] active tree fullHistory idx = memo
--     go memo history active tree fullHistory idx =
--       let
--         ((current, cIdx), (older, oIdx)) = splitHistoryClean history idx
--         next = lookup active current :: Maybe ParseLeaf
--         next' = maybe (addChild tree active current) next
--       in
--         undefined
--
--           if (next.nonEmpty) next.obsCount += 1
--              next.addLocation(cIdx)
--              go(older, next, tree, fullHistory, oIdx)

--dataWithIndex :: DataFileContents -> [(Int, Event)]
--dataWithIndex = zipWith
--    for (seq <- xs.view
--      .iterator
--      .filterNot(invalid.contains)
--      .zipWithIndex
--      .sliding(n+1)
--      .withPartial(false)) {
--      val obs = seq.map(_._1).mkString
--      val idxs = seq.map(_._2)
--      if (checkpoints.contains(idxs.head)) {
--        info(s"${idxs.head / tree.dataSize * 100}% of data streamed")
--      }
--      cleanInsert(tree, obs, idxs)
--    }



{-
  def loadData(tree:ParseTree, xs: Array[Event], n: Int): ParseTree = {
    val invalid = "\r\n".toSet
    // terrible for something we can probably fuse into the following:
    val filteredCharactersCount = xs.count(invalid.contains)

    tree.maxLength = n
    tree.dataSize = xs.length - filteredCharactersCount
    tree.adjustedDataSize =  xs.length - filteredCharactersCount
    val checkpoints:Set[Double] = (1 until 4).map(i => tree.dataSize * i / 4 ).toSet

    for (seq <- xs.view
      .iterator
      .filterNot(invalid.contains)
      .zipWithIndex
      .sliding(n+1)
      .withPartial(false)) {
      val obs = seq.map(_._1).mkString
      val idxs = seq.map(_._2)
      if (checkpoints.contains(idxs.head)) {
        info(s"${idxs.head / tree.dataSize * 100}% of data streamed")
      }
      cleanInsert(tree, obs, idxs)
    }

    // mostly for test cases.
    val last:Int = if (n > tree.adjustedDataSize) tree.adjustedDataSize.toInt else n

    for (i <- (0 to last).reverse) {
      val left = xs.take(i).filterNot(invalid.contains)
      val lIdxs = 0 until i
      cleanInsert(tree, left , lIdxs )
    }

    info("data streaming complete")

    // calculate conditional histories
    for (depth <- 0 to n) {
      tree.getDepth(depth).foreach(_.calcNextStepProbabilities(tree))
    }

    // remove the final depth so that we are left only with predictive distributions. Note that this isn't strictly necessary.
    tree.getDepth(n).foreach{ _.children = ListBuffer() }
    tree
  }
-}

{-
type CurrentHistory = (Event, Int)
type OlderHistory = (Iterable[Event], Iterable[Int])
protected def splitHistoryClean (observed: Iterable[Event], idx:Iterable[Int]): (CurrentHistory , OlderHistory) = {
  ((getCurrent(observed), getCurrent(idx)), (getPrior(observed), getPrior(idx)))
}

type GetCurrent[A >: AnyVal] = (List[A])=>A
type GetPrior[A >: AnyVal] = (List[A])=>List[A]

cleanInsert :: Foldable f
            => PLeaf
            -> f Event
            -> f Int
            -> PLeaf
cleanInsert tree observed idx = go observed (root tree) tree observed idx
  where
    go :: Foldable f
       -> PLeaf
       -> f Event
       -> ParseLeaf
       -> PLeaf
       -> [Event]
       -> f Int
       -> PLeaf
    go memo      [] active tree fullHistory idx = memo
    go memo history active tree fullHistory idx =
      let
        ((current, cIdx), (older, oIdx)) = splitHistoryClean history idx
        next = lookup active current :: Maybe ParseLeaf
        next' = maybe (addChild tree active current) next
      in
        undefined

--           if (next.nonEmpty) next.obsCount += 1
--              next.addLocation(cIdx)
--              go(older, next, tree, fullHistory, oIdx)
  -}


