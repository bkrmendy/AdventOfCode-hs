{-# LANGUAGE FlexibleInstances #-}

module Year2018.Day7 where
import Challenge
import Utils (parseLines, insertL, ensureE)
import Data.Char (ord)
import Data.List (partition, nub)
import Debug.Trace
import qualified Data.Map as Map
import Text.Parsec

data Depends = Depends { _before :: Char, _after :: Char }

newtype Graph = Graph { _unGraph :: Map.Map Char [Char] } deriving Show

parseDependency :: Parsec String () Depends
parseDependency = Depends <$> (string "Step " *> letter)
                          <*> (string " must be finished before step " *> letter <* string " can begin.")

toGraph :: [Depends] -> Graph
toGraph = Graph . go Map.empty
  where
    go nodes [] = nodes
    go nodes (Depends part dependsOnPart:rest) = go (nextNodes nodes) rest
      where
        nextNodes = insertL dependsOnPart part . ensureE part . ensureE dependsOnPart

removeNode :: Char -> Graph -> Graph
removeNode k g@(Graph nodes) = case Map.lookup k nodes of
    Nothing -> g
    Just deps -> Graph newGraph
  where
    newGraph = Map.fromList [(key, filter (/= k) deps) | (key, deps) <- Map.assocs nodes, key /= k]

popNode :: Graph -> (Char, Graph)
popNode graph = (step, nextGraph)
  where
    step = head $ availableNodes graph
    nextGraph = removeNode step graph

availableNodes :: Graph -> [Char]
availableNodes = Map.keys . Map.filter null . _unGraph

topo :: Graph -> [Char]
topo g@(Graph nodes)
  | Map.null nodes = []
  | otherwise = step:topo nextGraph
    where (step, nextGraph) = popNode g

workTime :: Int -> Char -> Int
workTime i c = ord c - ord 'A' + i

kickOff :: (Char -> Int) -> Int -> Graph -> Int
kickOff costF nWorkers graph = construct [(c, costF c) | c <- avail] costF nWorkers 0 graph
  where avail = availableNodes graph

type JobQueue = [(Char, Int)]

construct :: JobQueue     -- ^ Job queue
          -> (Char -> Int)-- ^ cost function
          -> Int          -- ^ number of workers
          -> Int          -- ^ current time
          -> Graph        -- ^ graph
          -> Int          -- ^ total time
construct [] _ _ time _               = time
construct queue costF workers time graph = trace (show queue) $ construct nextQueue costF workers (time + 1) nextGraph
    where
      (done, inProgress) = partition ((<= 0) . snd) queue
      nextGraph = foldr (removeNode . fst) graph done
      outs = availableNodes nextGraph
      inProgressNodes = map fst inProgress
      nextQueue = advance inProgress <> [(o, costF o) | o <- filter (`notElem` inProgressNodes) (nub outs)]
      advance q = [(c, i - 1) | (c, i) <- take workers q] <> drop workers q

instance Challenge [Depends] where
  parse = parseLines (sepBy1 parseDependency newline)
  partOne = topo . toGraph
  partTwo = show . kickOff (workTime 60) 5 . toGraph