{-# LANGUAGE FlexibleInstances #-}

module Year2018.Day7 where
import Challenge
import Utils (parseLines)
import Data.Char (ord)
import qualified Data.Map as Map

import Text.Parsec

data Depends = Depends { _before :: Char, _after :: Char }

data Node = Node { _ins :: [Char], _outs :: [Char] }

newtype Graph = Graph { _nodes :: Map.Map Char Node }

parseI :: Parsec String () Depends
parseI = Depends <$> (string "Step " *> letter) <*> (string " must be finished before step " *> letter <* string " can begin.")

putInEdge :: Char -> Char -> Graph -> Graph
putInEdge from to (Graph nodes) = Graph $ Map.insert to (Node (from:ins) outs) nodes
  where
    (Node ins outs) = Map.findWithDefault (Node [] []) to nodes

putOutEdge :: Char -> Char -> Graph -> Graph
putOutEdge from to (Graph nodes) = Graph $ Map.insert from (Node ins (to:outs)) nodes
  where
   (Node ins outs) = Map.findWithDefault (Node [] []) from nodes

insertGraph :: Depends -> Graph -> Graph
insertGraph (Depends finished before) = putInEdge finished before . putOutEdge finished before

toGraph :: [Depends] -> Graph
toGraph deps = go deps (Graph Map.empty)
  where
    go []         graph = graph
    go (dep:rest) graph = go rest (insertGraph dep graph)

removeFromGraph :: Char -> Graph -> (Node, Graph)
removeFromGraph k (Graph nodes) = (node, Graph newGraph)
  where
    node = nodes Map.! k
    newGraph = Map.fromList [(key, Node (filter (/= k) ins) outs) | (key, Node ins outs) <- Map.assocs nodes
                                                                  , key /= k]

popNode :: Graph -> (Char, Node, Graph)
popNode g@(Graph nodes) = (step, node, nextGraph)
  where
    (step, node) = Map.findMin (Map.filter (null . _ins) nodes)
    (_, nextGraph) = removeFromGraph step g

topo :: Graph -> [Char]
topo g@(Graph nodes)
  | Map.null nodes = []
  | otherwise = step:topo nextGraph
    where
      (step, _, nextGraph) = popNode g

workTime :: Char -> Int
workTime c = 61 + ord c - ord 'A'

type JobQueue = (Char, Int)

kickOff :: Int -> Graph -> Int
kickOff nWorkers graph = construct [(c, workTime c)] nWorkers 0 graph
  where
    (c, _, _) = popNode graph

peel :: [Char] -> Graph -> [Char]
peel [] _ = []
peel (c:rest) graph = outs <> peel rest g
  where
     (Node _ outs, g) = removeFromGraph c graph 

construct :: [JobQueue]   -- ^ Job queue
          -> Int          -- ^ number of workers
          -> Int          -- ^ current time
          -> Graph        -- ^ graph
          -> Int          -- ^ total time
construct [] _ time _ = time
construct queue workers time graph
  | null (done queue) = construct (advance (take workers queue) <> drop workers queue) workers (time + 1) graph
  | otherwise = construct (advance (nuQueue queue)) workers (time + 1) graph
    where
      advance q = [(c, i - 1) | (c, i) <- take workers q] <> drop workers q
      done q = [c | (c, i) <- q, i < 0]
      outs = foldr _ graph (done queue)
      nuQueue q = [(c, i) | (c, i) <- q, i >= 0] <> outs
      

instance Challenge [Depends] where
  parse = parseLines (sepBy1 parseI newline)
  partOne = topo . toGraph
  partTwo = show . kickOff 5 . toGraph