module Year2018.Day8 where
import Challenge
import Utils (readInt)

import Control.Monad.State
import Data.Maybe (catMaybes)
import qualified Data.Vector as V

data Node = Node { _metadata :: V.Vector Int, _children :: V.Vector Node }

pop :: Int -> State [Int] [Int]
pop n = do
  xs <- gets $ take n
  modify' $ drop n
  return xs

parseMeta :: Int -> State [Int] [Int]
parseMeta = pop

nodeI :: State [Int] Node
nodeI = do
  info <- pop 2
  children <- forM [1..(head info)] $ const nodeI
  meta <- pop (info !! 1)
  return $ Node (V.fromList meta) (V.fromList children)

node :: [Int] -> Node
node = evalState nodeI 

metaSum :: Node -> Int
metaSum (Node m children) = sum m + sum (V.map metaSum children)

value :: Node -> Int
value (Node m ch)
  | V.null ch = V.sum m
  | otherwise = sum . catMaybes $ [value <$> (ch V.!? (n - 1)) | n <- V.toList m]

instance Challenge Node where
  parse = node . map readInt . words
  partOne = show . metaSum
  partTwo = show . value
