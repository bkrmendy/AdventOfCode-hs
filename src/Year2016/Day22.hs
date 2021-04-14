{-# LANGUAGE FlexibleInstances #-}
module Day22 where
import Text.Parsec
import Challenge
import qualified Data.Map as Map
import Data.List (sortOn)
import Utils

data Node = Node { coord :: (Int, Int)
                 , size :: Int
                 , used :: Int
                 , avail :: Int
                 , usePercent :: Int
                 } deriving (Eq, Show)


-- /dev/grid/node-x0-y0     87T   71T    16T   81%
parseNode :: Parsec String () Node
parseNode = Node  <$> ((,) <$> (string "/dev/grid/node-x" *> int) <*> (string "-y" *> int)) -- coord
                  <*> (spaces *> int <* char 'T') -- size
                  <*> (spaces *> int <* char 'T') -- used
                  <*> (spaces *> int <* char 'T') -- avail
                  <*> (spaces *> int <* char '%') -- usePercent

viable :: Node -> Node -> Bool
viable a b = used a > 0 && used a <= avail b

partOneI :: [Node] -> Int
partOneI nodes = length [() | a <- nodes
                            , b <- nodes
                            ,  a /= b
                            , viable a b]

fromList :: [Node] -> [[Node]]
fromList nodes = [[grid Map.! (x, y) | x <- [0..36]] | y <- [0..24]]
  where
    grid = Map.fromList (map (\node -> (coord node, node)) nodes)

fromNode :: Node -> Char
fromNode node
  | coord node == (36, 0) = 'G'
  | usePercent node >= 90 = '#'
  | avail node >= 64 = '_'
  | otherwise = '.'

showGrid :: [[Node]] -> [String]
showGrid = map (map fromNode)

-- ^ part 2 inspired by reddit
partTwoI :: [Node] -> String
partTwoI =  unlines . showGrid . fromList

-- ^ 👀 3 + 17 + 31 + 35 * 5 + 1
instance Challenge [Node] where
  parse = parseLines (sepBy1 parseNode newline)
  partOne = show . partOneI
  partTwo = partTwoI