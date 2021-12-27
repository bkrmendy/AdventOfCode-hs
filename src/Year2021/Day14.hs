module Year2021.Day14 where

import Challenge
import Data.List (foldl')
import Data.List.Split (splitOn)

import qualified Data.Map.Strict as M  

type Templates = M.Map (Char, Char) Char
type Polymerization = (String, Templates)

template :: String -> ((Char, Char), Char)
template input = let [[a, b], [c]] = splitOn " -> " input in ((a, b), c)

parseI :: String -> Polymerization
parseI input = (start, M.fromList $ template <$> lines rules)
  where [start, rules] = splitOn "\n\n" input
  
pairs :: String -> [(Char, Char)]
pairs [a, b] = [(a, b)]
pairs (a:b:rest) = (a, b) : pairs (b:rest)

type PairCount = M.Map (Char, Char) Int

pairCount :: String -> PairCount
pairCount seed = M.fromListWith (+) [(p, 1) | p <- pairs seed]

react :: Templates -> PairCount -> PairCount
react templates pc = foldl' go M.empty (M.assocs pc) 
  where
    go nextPc (pair, numberOfPairs) = case M.lookup pair templates of
      Nothing -> M.insertWith (+) pair numberOfPairs nextPc
      Just ie -> M.insertWith (+) (fst pair, ie) numberOfPairs
                $ M.insertWith (+) (ie, snd pair) numberOfPairs nextPc

evaluate :: Char -> Char -> PairCount -> Int
evaluate first end pc = uncurry (-)
                      $ bounds
                      $ M.elems
                      $ M.insertWith (+) first 1
                      $ M.insertWith (+) end 1
                      $ foldl' go M.empty (M.assocs pc)
  where go mc ((a, b), c) = M.insertWith (+) a c $ M.insertWith (+) b c mc
        bounds mc = (maximum mc `div` 2, minimum mc `div` 2)
        
run :: Int -> Polymerization -> Int
run steps (seed, templates) = evaluate first end
                            $ last
                            $ take (steps + 1) 
                            $ iterate (react templates) (pairCount seed)
  where (first, end) = (head seed, last seed)

instance Challenge Polymerization where
  parse = parseI
  partOne = show . run 10
  partTwo = show . run 40
