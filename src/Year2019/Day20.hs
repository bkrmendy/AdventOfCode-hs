module Year2019.Day20 where

import Challenge
import Utils (inserts)

import Prelude hiding (lookup)

import Data.Maybe (mapMaybe)
import Data.List (find)

import           Data.Array.Unboxed ((!))
import qualified Data.Array.Unboxed as A

import qualified Data.Map.Strict as M
import qualified Data.Set as S

import           Data.Sequence (ViewL(..), (><))
import qualified Data.Sequence as Q

import Data.Bifunctor (second)

import Control.Monad.State.Strict

import qualified Debug.Trace as T

-- | Type aliases
type Position = (Int, Int)
data Direction = Inward | Outward deriving (Eq, Ord, Show)
data Portal = Portal
  { _pdir :: Direction
  , _pdepth :: Int
  , _ppos :: Position
  , _pname :: String
  } deriving (Eq, Ord, Show)

type Distance = (Int, Portal)
type Edges = M.Map Position (S.Set Distance)

type Chart = A.UArray Position Char

-- | Portals
type Portals = S.Set Portal

-- | Chart
mkChart :: String -> Chart
mkChart input = A.array ((0, 0), (rs, cs)) [((row, col), (ls !! row) !! col) | row <- [0..rs], col <- [0..cs]]
  where ls = lines input
        (rs, cs) = (length ls - 1, length (head ls) - 1)

-- | Portals
isPortalChar :: Char -> Bool
isPortalChar c = c `elem` cs
  where cs = ['A'..'Z']

isOuter :: (Int, Int) -> (Int, Int) -> Bool
isOuter (rs, cs) (r, c) = r <= 2 || r >= (rs - 2) || c <= 2 || c >= (cs - 2) 

windowF :: (Int, Int) -> ((Position, Char), (Position, Char), (Position, Char)) -> Maybe Portal
windowF bounds ((pa, a), (_, b), (pc, c))
  | isPortalChar a && isPortalChar b && c == '.' = Just (make pc [a, b])
  | isPortalChar b && isPortalChar c && a == '.' = Just (make pa [b, c])
  | otherwise  = Nothing
  where make :: Position -> String -> Portal
        make p n | isOuter bounds p = Portal Outward 0 p n
                 | otherwise = Portal Inward 0 p n

portals :: Chart -> Portals
portals chart = S.fromList ps
  where (_, (rs, cs)) = A.bounds chart
        rows = [(pick row col, pick row (col + 1), pick row (col + 2)) | col <- [0..cs - 2], row <- [0..rs]]
        cols = [(pick row col, pick (row + 1) col, pick (row + 2) col) | col <- [0..cs], row <- [0..rs - 2]]
        pick r c = ((r, c), chart ! (r, c))
        ps = mapMaybe (windowF (rs, cs)) (rows ++ cols) 
        
-- | Edges
open :: Chart -> Position -> Bool
open chart pos = chart ! pos == '.'

tile :: Chart -> Position -> Maybe Char
tile chart pos
  | A.inRange (A.bounds chart) pos = Just (chart ! pos)
  | otherwise = Nothing

neighbors :: Position -> Chart -> [Position]
neighbors (row, col) chart = filter (A.inRange (A.bounds chart)) [(row + 1, col), (row - 1, col), (row, col + 1), (row, col - 1)]

data BFSState = BFSState { _chart :: Chart, _portals :: Portals, _next :: Q.Seq (Position, Int), _seen :: S.Set Position }

trace :: Portals -> Position -> Int -> [Distance]
trace ps pos d = [(d, po) | po <- S.elems ps, _ppos po == pos]

bfsI :: State BFSState [Distance]
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

bfs :: Chart -> Portals -> Portal -> [Distance]
bfs chart ps (Portal _ _ pos _) = evalState bfsI (BFSState chart ps (Q.singleton (pos, 0)) (S.singleton pos))

pair :: Portal -> Portal -> Bool
pair (Portal da _ _ na) (Portal db _ _ nb) = da /= db && na == nb

edgesFrom :: Chart -> Portals -> Portal -> (Position, S.Set Distance)
edgesFrom chart ps p = (_ppos p, edgesWithPair)
  where es = S.filter ((/= p)  . snd) . S.fromList $ bfs chart ps p
        warp = find (pair p) (S.elems ps)
        edgesWithPair = S.union (maybe S.empty (\o -> S.singleton (1, o)) warp) es

edges :: Chart -> Portals -> Edges
edges chart ps = M.fromList $ map (edgesFrom chart ps) (S.elems ps)

type StepFn = Distance -> [Distance]

-- ^ adapted from https://stackoverflow.com/a/64322395/16213214
spfa
    :: StepFn                         -- ^ Where we can go from a node and the cost of that
    -> Portal                         -- ^ Where we want to get to
    -> Distance                       -- ^ The start position
    -> Maybe Distance                 -- ^ Maybe the answer. Maybe it doesn't exist
spfa next target start = search mempty (S.singleton start)
  where
    search visited toBeVisited = case S.minView toBeVisited of
      Nothing -> Nothing
      Just ((cost, vertex), withoutVertex)
        | _pdepth vertex > 1000       -> error "recursion limit exceeded!"
        | vertex == target            -> Just (cost, vertex)
        | vertex `S.member` visited   -> search visited withoutVertex
        | otherwise                   -> search visitedWithNode withNext
        where
          visitedWithNode = S.insert vertex visited
          withNext = foldr S.insert withoutVertex $ next (cost, vertex)

step1 :: Edges -> StepFn
step1 es (distance, portal) = [(distance + d, nextPortal) | (d, nextPortal) <- ns]
  where ns = S.elems (es M.! _ppos portal)

setDepth :: Int -> Portal -> Portal
setDepth d (Portal dir _ pos n) = Portal dir d pos n

deepen :: Direction -> Portal -> Int -> Portal
deepen Inward (Portal dir _ pos n) d = Portal dir (d + 1) pos n
deepen Outward (Portal dir _ pos n)  d = Portal dir (d - 1) pos n

step2 :: Edges -> StepFn
step2 es (distance, portal) = T.traceShow (portal, zs) zs 
  where depth = _pdepth portal
        ns = S.elems (es M.! _ppos portal)
        zs = mapMaybe (uncurry $ next (_pdepth portal)) (map (second (setDepth depth)) ns)
              
        next :: Int -> Int -> Portal -> Maybe (Int, Portal)
        next 0 di p@(Portal _ _ _ "AA") = Just (distance + di, p) 
        next _ _    (Portal _ _ _ "AA") = Nothing
        next 0 di p@(Portal _ _ _ "ZZ") = Just (distance + di, p)
        next _ _    (Portal _ _ _ "ZZ") = Nothing
        
        next de di p | pair p portal = Just (distance + di, deepen (_pdir portal) p de)
        next 0 _    (Portal Outward _ _ _) = Nothing
        
        next _  di p = Just (distance + di, p)
        
solve :: (Edges -> StepFn) -> Chart -> Maybe Int
solve step chart = fst <$> spfa (step es) end (0, start)
  where ps    = portals chart
        es    = edges chart ps
        start = head . filter ((== "AA") . _pname) $ S.elems ps
        end   = head . filter ((== "ZZ") . _pname) $ S.elems ps

instance Challenge Chart where
  parse = mkChart
  partOne = show . solve step1
  partTwo = show . solve step2
