{-# LANGUAGE OverloadedLists #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE KindSignatures #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE InstanceSigs #-}
{-# LANGUAGE PatternSynonyms #-}
{-# LANGUAGE ViewPatterns #-}

module Data.Parse.Tree
  -- ( PLeaf
  -- ) where
  where

import GHC.TypeLits
import Data.Proxy
import Control.Monad.ST
import Data.STRef
import Control.Exception (assert)
import qualified Data.HashSet as HS
import qualified Data.HashMap.Strict as HM
import qualified Data.Vector as V
import Data.Hashable
import Lens.Micro.Internal

import CSSR.Prelude
import CSSR.TypeAliases

data ParseTree (n :: Nat)  = ParseTree
  { root :: PLeaf
  } deriving (Show, Eq)

-- data LoopingTree = LoopingTree
--   { root :: LLeaf
--   , terminals:: Set LLeaf
--   , edges :: Set LLeaf
--   } deriving (Show, Eq)
--
-- data LLeaf = LLeaf
--   { obs       :: Vector Event
--   , histories :: Set PLeaf
--   , children  :: HashMap Char LLeaf
--   , distribution :: Vector Float
--   , isTerminal :: Bool
--   , isEdge :: Bool
--   , isLoop :: Bool
--   }

height :: forall n . KnownNat n => ParseTree n -> Integer
height _ = natVal @n Proxy

data PLeaf = PLeaf
  { _body :: PLeafBody
  , _children :: Children
  } deriving (Show, Eq)

type instance Index PLeaf = Vector Event
type instance IxValue PLeaf = PLeaf

instance Ixed PLeaf where
  ix :: Vector Event -> Traversal' PLeaf (IxValue PLeaf)
  ix histories = go 0
    where
      go depth f p@(PLeaf body childs)
        | V.length histories == depth = f p
        | otherwise =
          case HM.lookup c childs of
            Nothing -> pure p
            Just child -> goAgain <$> go (depth+1) f child
        where
          c :: Event
          c = histories V.! depth

          goAgain :: PLeaf -> PLeaf
          goAgain child' = PLeaf body (HM.insert c child' childs)

-- make a note: steal mitchell's brain
-- study all of the monoids
-- do in-depth dives for each of the lens modules

data PLeafBody = PLeafBody
  { _obs       :: Vector Event
  , _count     :: Integer
  , _locations :: Locations
  } deriving (Show, Eq)


type Event = Char
type Children = HashMap Event PLeaf
type Parent = Maybe PLeaf
-- type Alphabet = [Event]

makeLenses ''PLeafBody
makeLenses ''PLeaf















current :: Vector Event -> Event
current = V.last

prior :: Vector Event -> Vector Event
prior = V.init

currentInv :: Vector Event -> Event
currentInv = V.head

priorInv :: Vector Event -> Vector Event
priorInv = V.tail

mkRoot :: PLeaf
mkRoot = PLeaf (PLeafBody [] 0 mempty) mempty

mkLeaf :: Vector Event -> PLeaf
mkLeaf obs = PLeaf (PLeafBody obs 0 mempty) mempty

navigate :: PLeaf -> Vector Event -> Maybe PLeaf
navigate lf history = lf ^? ix history

-- addLocation :: PLeaf -> Idx -> PLeaf
-- addLocation lf i = over locations (increment i) lf
--   where
--     increment :: Idx -> Locations -> Locations
--     increment i locs = HM.insertWith (+) i 1 locs

---------------------------------------------------------------------------------
-- We encounter the history say "110"
-- We go to the parse tree at the root
-- We take the 0 child of the root
-- We then take the 1 child of 0 (=10)
-- We then take the 1 child of 10 (=110)
--------------------------------------------------------------------------------
--
-- mkRoot & over (path (fromList "abc") . count) (+1)

path :: forall f. Applicative f
             => Vector Event
             -> (PLeafBody -> f PLeafBody)
             -> PLeaf
             -> f PLeaf
path events fn = _path 0
  where
    _path :: Int -> PLeaf -> f PLeaf
    _path depth (PLeaf body childs) =
      assert (V.take depth events ==  _obs body) $
        if depth == V.length events - 1
        then PLeaf <$> fn body <*> pure childs
        else PLeaf <$> fn body <*> nextChilds

      where
        nextChilds :: f (HashMap Event PLeaf)
        nextChilds =
          let
            c = V.unsafeIndex events depth  :: Event
            nextDepth = depth + 1 :: Int
          in
            case HM.lookup c childs of
              Just child -> HM.insert c <$> _path (depth + 1) child <*> pure childs
              Nothing -> HM.insert c <$> buildNew depth <*> pure childs


        buildNew :: Int -> f PLeaf
        buildNew depth
          | depth == V.length events - 1 = PLeaf <$> newBody events <*> pure mempty
          | otherwise = PLeaf <$> newBody es <*> childs
          where
              c :: Event
              c = V.unsafeIndex events (depth + 1)

              es :: Vector Event
              es = V.take (depth + 1) events

              newBody :: Vector Event -> f PLeafBody
              newBody es' = fn (PLeafBody es' 0 mempty)

              childs :: f (HashMap Char PLeaf)
              childs = HM.singleton c <$> buildNew (depth + 1)































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

-- buildBranch :: Vector Event -> IO PLeaf
-- buildBranch [] parent = return mkRoot
-- buildBranch es parent = do
--   let e   = currentInv es
--       es' = priorInv es
--
--   undefined -- V.cons mkRoot $ V.map
--  where
--    e :: Event
--    e = current es
--
--    newLeaf :: PLeaf
--    newLeaf = mkLeaf (V.snoc _obs e)
--
--    child :: PLeaf
--    child = maybe newLeaf (over count (+1)) $ findChild active e

buildTree :: DataFileContents -> (Int, PLeaf)
buildTree chars = go chars root (0, root)
  where
    root :: PLeaf
    root = mkRoot

    go :: DataFileContents -> PLeaf -> (Int, PLeaf) -> (Int, PLeaf)
    go alldata active (nFiltered, root) = undefined

loadData :: DataFileContents -> Int -> ParseTree n
loadData chars n = ParseTree
  { root = undefined -- root
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

