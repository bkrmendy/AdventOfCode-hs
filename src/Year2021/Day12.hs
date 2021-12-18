module Year2021.Day12 where

import Challenge  

import Data.Char (isUpper, isLower)
import Data.List.Split (splitOn)

import qualified Data.Set as S

import Control.Monad (guard)

type Graph = [(String, String)]

parseI :: String -> Graph
parseI = concatMap edge . lines
  where edge e = let [a, b] = splitOn "-" e in [(a, b), (b, a)]
  
big :: String -> Bool
big (c:_) = isUpper c

type Seen = S.Set String

edgesFrom :: Graph -> String -> [String]
edgesFrom graph e = map snd $ filter (\(a, _) -> a == e) graph
  
paths :: Graph -> [[String]]
paths graph = pathsI False "start" (S.singleton "start")
  where
    pathsI :: Bool -> String -> Seen -> [[String]]
    pathsI v cave seen | cave == "end" = [["end"]]
                       | otherwise = do
                          edge <- edgesFrom graph cave
                          if not (edge `S.member` seen) || big edge
                           then (cave:) <$> pathsI v edge (edge `S.insert` seen)
                           else if not (big edge) && edge /= "start" && not v
                            then (cave:) <$> pathsI True edge seen
                            else mempty

-- | TODO: somehow factor out the second part (`part2` flag isn't valid)
instance Challenge Graph where
  parse = parseI
  partOne = show . length . paths
