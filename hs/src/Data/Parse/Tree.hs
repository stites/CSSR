{-# LANGUAGE OverloadedLists #-}

module Data.Parse.Tree
  -- ( PLeaf
  -- ) where
  where

import qualified Data.HashSet as HS
import qualified Data.HashMap.Strict as HM
import Lens.Micro.Platform -- FIXME: why can't I include this in CSSR.Prelude???

import CSSR.Prelude
import CSSR.TypeAliases

data ParseTree = ParseTree
  { maxLength :: Int
  , dataSize :: Double
  , root :: PLeaf
  } deriving (Show, Eq)


data PLeaf = PLeaf
  { obs :: String
  , count :: Integer
  , parent :: Parent
  , children :: Children
  , locations :: Locations
  } deriving (Show, Eq)

current :: [Char] -> Char
current = last

prior :: [Char] -> [Char]
prior = init


type Children = HashMap Char PLeaf
type Parent = Maybe PLeaf

mkRoot :: PLeaf
mkRoot = PLeaf "" 0 Nothing mempty mempty

findChild :: PLeaf -> Char -> Maybe PLeaf
findChild lf c = HM.lookup c (children lf)

navigate :: [Char] -> PLeaf -> Maybe PLeaf
navigate      [] active = Just active
navigate history active =
  case (findChild active current') of
    Just next -> navigate prior' next
    Nothing   -> Nothing

  where
    current' :: Char
    current' = current history

    prior' :: [Char]
    prior' = prior history


addLocation :: PLeaf -> Idx -> PLeaf
addLocation lf i = _locations (increment i) lf
  where
    increment :: Idx -> Locations -> Locations
    increment i locs = HM.insertWith (+) i 1 locs

    _locations :: (Locations -> Locations) -> PLeaf -> PLeaf
    _locations f p = PLeaf
      {       obs = obs p
      ,     count = count p
      ,    parent = parent p
      ,  children = children p
      , locations = f (locations p)
      }

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

loadData :: PLeaf -> [Char] -> Int -> PLeaf
loadData tree chars n = undefined
  where
    banned :: HashSet Char
    banned = HS.fromList "\r\n"

    filtered :: [Char]
    filtered = filter (\x -> not (HS.member x banned)) chars


{-
type CurrentHistory = (Char, Int)
type OlderHistory = (Iterable[Char], Iterable[Int])
protected def splitHistoryClean (observed: Iterable[Char], idx:Iterable[Int]): (CurrentHistory , OlderHistory) = {
  ((getCurrent(observed), getCurrent(idx)), (getPrior(observed), getPrior(idx)))
}

type GetCurrent[A >: AnyVal] = (List[A])=>A
type GetPrior[A >: AnyVal] = (List[A])=>List[A]

cleanInsert :: Foldable f
            => PLeaf
            -> f Char
            -> f Int
            -> PLeaf
cleanInsert tree observed idx = go observed (root tree) tree observed idx
  where
    go :: Foldable f
       -> PLeaf
       -> f Char
       -> ParseLeaf
       -> PLeaf
       -> [Char]
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
