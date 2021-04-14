{-# LANGUAGE FlexibleInstances #-}
module Year2015.Day3 where
import Challenge
import qualified Data.Map as Map
import Control.Monad.State

type Houses = Map.Map (Int, Int) Int

give :: (Int, Int) -> Houses -> Houses
give pos = Map.insertWith (\a _ -> a + 1) pos 1

move :: Char -> (Int, Int) -> (Int, Int)
move '<' (c, r) = (c - 1, r    )
move '^' (c, r) = (c    , r - 1)
move 'v' (c, r) = (c    , r + 1)
move '>' (c, r) = (c + 1, r    )

step :: (Int, Int) -> [Char] -> State Houses ()
step _ [] = return ()
step pos (dir:dirs) = do
  modify (give pos)
  step (move dir pos) dirs
  
nonZero :: Houses -> Int
nonZero = Map.size . Map.filter (> 0)

turns :: [a] -> ([a], [a])
turns xs = (choose even, choose odd)
  where
    xsn = zip [1..] xs
    choose f = map snd $ filter (\(i, _) -> f i) xsn

robo :: [Char] -> State Houses ()
robo dirs = do
  (santa, robot) <- pure $ turns dirs
  step (0, 0) santa
  step (0, 0) robot

instance Challenge [Char] where
  parse = id
  partOne dirs = show . nonZero $ execState (step (0, 0) dirs) Map.empty
  partTwo dirs = show . nonZero $ execState (robo dirs) Map.empty  
   

