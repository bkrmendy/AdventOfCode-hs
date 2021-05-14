{-# LANGUAGE MultiWayIf #-}
module Year2017.Day24 where
import Challenge
import Utils                      (readInt, select)

import Control.Applicative        (Alternative(..))
import Control.Monad.Trans.State  (StateT(..), evalStateT)
import Data.List.Split            (splitOn)
import Data.Ord                   (Down(..))
import Data.Tuple                 (swap)
import Data.Bifunctor             (first)

type Connector = (Int, Int)

connector :: String -> Connector
connector line = (readInt a, readInt b)
  where [a, b] = splitOn "/" line

-- | based on: https://blog.jle.im/entry/unique-sample-drawing-searches-with-list-and-statet.html

bridge :: Int -> StateT [Connector] [] Int
bridge from = do
  (x, y) <- StateT select
  next <- if | x == from -> return y
             | y == from -> return x
             | otherwise -> empty
  rest <- return 0
       <|> bridge next
  return $ x + y + rest

pt1 :: [Connector] -> Int
pt1 = maximum . evalStateT (bridge 0)

pt2 :: [Connector] -> Int
pt2 = snd . maximum
     . map (first (Down . length) . swap)
     . runStateT (bridge 0)

instance Challenge [Connector] where
  parse = map connector . lines
  partOne = show . pt1
  partTwo = show . pt2
