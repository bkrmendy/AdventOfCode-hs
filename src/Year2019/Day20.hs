module Year2019.Day20 where

import Challenge
import Utils (inserts)

import Prelude hiding (lookup)

import Data.Maybe (mapMaybe)
import Data.Char (isLetter)
import Data.List (find)

import           Data.Array.Unboxed ((!))
import qualified Data.Array.Unboxed as A

import qualified Data.Map.Strict as M
import qualified Data.Set as S

import           Data.Sequence (ViewL(..), (><))
import qualified Data.Sequence as Q

import Control.Monad.State.Strict

-- | Type aliases
type Position = (Int, Int)
data Portal = Portal Position String deriving (Eq, Ord)

type Distance = (String, Int)
type Edges = M.Map Portal [Distance]

type Chart = A.UArray Position Char

-- | Portals
data Portals = Portals { _position :: M.Map String Position, _name :: M.Map Position String }

name :: Position -> Portals -> Maybe Portal
name p (Portals _ ps) =
  case M.lookup p ps of
    Nothing -> Nothing
    Just na -> Just (Portal p na) 

position :: String -> Portals -> Maybe Portal
position n (Portals ns _) =
  case M.lookup n ns of
    Nothing -> Nothing
    Just po -> Just (Portal po n)

elems :: Portals -> [Portal]
elems (Portals _ ns) = [Portal p n | (p, n) <- M.assocs ns]

-- | Chart
mkChart :: String -> Chart
mkChart input = A.array ((0, 0), (rs, cs)) [((row, col), (ls !! row) !! col) | row <- [0..rs], col <- [0..cs]]
  where ls = lines input
        (rs, cs) = (length ls - 1, length (head ls) - 1)

-- | Portals
windowF :: ((Position, Char), (Position, Char), (Position, Char)) -> Maybe Portal
windowF ((pa, a), (_, b), (pc, c))
  | isLetter a && isLetter b = Just (Portal pc [a, b])
  | isLetter b && isLetter c = Just (Portal pa [b, c])
  | otherwise  = Nothing

portals :: Chart -> Portals
portals chart = Portals (M.fromList [(n, p) | Portal p n <- ps]) (M.fromList [(p, n) | Portal p n <- ps])
  where (_, (rs, cs)) = A.bounds chart
        rows = [(pick row col, pick row (col + 1), pick row (col + 2)) | col <- [0..cs - 2], row <- [0..rs]]
        cols = [(pick row col, pick (row + 1) col, pick (row + 2) col) | col <- [0..cs], row <- [0..rs - 2]]
        pick r c = ((r, c), chart ! (r, c))
        ps = mapMaybe windowF (rows ++ cols) 
        
-- | Edges
large :: Int
large = 1000000000

open :: Chart -> Position -> Bool
open chart pos = chart ! pos == '.'

tile :: Chart -> Position -> Maybe Char
tile chart pos
  | A.inRange (A.bounds chart) pos = Just (chart ! pos)
  | otherwise = Nothing

neighbors :: Position -> Chart -> [Position]
neighbors (row, col) chart = filter (A.inRange (A.bounds chart)) [(row + 1, col), (row - 1, col), (row, col + 1), (row, col - 1)]

data BFSState = BFSState { _chart :: Chart, _portals :: Portals, _next :: Q.Seq (Position, Int), _seen :: S.Set Position }

trace :: Portals -> Position -> Int -> [(String, Int)]
trace ps pos d =
  case name pos ps of
    Nothing -> []
    Just (Portal _ na) -> [(na, d)]

bfsI :: State BFSState [(String, Int)]
bfsI = do
  (BFSState c p n s) <- get
  case Q.viewl n of
    Q.EmptyL -> return []
    (t, d) :< rest -> do
      let neighbos = filter (\po -> po `S.notMember` s && open c po) (neighbors t c)
          nextNext = [(po, d + 1) | po <- neighbos]
          res = trace p t d 
      put (BFSState c p (rest >< Q.fromList nextNext) (inserts neighbos s))
      restOfBfs <- bfsI
      return (res ++ restOfBfs)

bfs :: Chart -> Portals -> Portal -> [(String, Int)]
bfs chart ps (Portal pos _) = evalState bfsI (BFSState chart ps (Q.singleton (pos, 0)) (S.singleton pos))

edges :: Chart -> Portals -> Edges
edges chart ps = M.fromList $ map (\p -> (p, bfs chart ps p)) (elems ps)

lookup :: String -> [Distance] -> Maybe Int
lookup na ds = snd <$> find (\(n, _) -> n == na) ds

-- | Relax
-- | based on Dijkstra
relax :: Portals -> Edges -> Portal -> [(String, Int)]
relax ps es p = undefined

partOneI :: Chart -> Maybe Int
partOneI chart =
  let
    ps = portals chart
    es = edges chart ps
  in case position "AA" ps of
    Nothing -> error "AA not in portals"
    Just po -> lookup "ZZ" (relax ps es po)

instance Challenge Chart where
  parse = mkChart
  partOne = show . partOneI
