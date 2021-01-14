{-# LANGUAGE FlexibleInstances #-}
module DayTwentyTwo where
import Text.Parsec
import Data.List (sortOn, find)
import qualified Data.HashMap as HM
import Challenge
import Utils

data Node = Node { coord :: (Int, Int)
                 , size :: Int
                 , used :: Int
                 , avail :: Int
                 , usePercent :: Int
                 } deriving (Eq)


-- /dev/grid/node-x0-y0     87T   71T    16T   81%
parseNode :: Parsec String () Node
parseNode = Node  <$> ((,) <$> (string "/dev/grid/node-x" *> int) <*> (string "-y" *> int)) -- coord
                  <*> (spaces *> int <* char 'T') -- size
                  <*> (spaces *> int <* char 'T') -- used
                  <*> (spaces *> int <* char 'T') -- avail
                  <*> (spaces *> int <* char '%') -- usePercent

viable :: Node -> Node -> Bool
viable a b = used a > 0 && used a <= avail b

solve :: [Node] -> Int
solve nodes = 42 -- wip

instance Challenge [Node] where
  parse = parseLines (sepBy1 parseNode newline)
  partOne nodes = show $ length [() | a <- nodes
                                    , b <- nodes
                                    ,  a /= b
                                    , viable a b]
  partTwo = show . solve