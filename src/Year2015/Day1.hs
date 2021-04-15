module Year2015.Day1 where
import Challenge
import Control.Monad.State

type Floor = Int -> Int

parseI :: Char -> Floor
parseI '(' = \f -> f + 1
parseI ')' = \f -> f - 1

walk :: [(Int, Floor)] -> State Int Int
walk [] = return (-1)
walk ((i, f):fs) = do
  currentFloor <- get
  if currentFloor < 0
    then return i
    else do
      put (f currentFloor)
      walk fs

partTwoI :: [Floor] -> Int
partTwoI fs = evalState (walk floors) 0
  where floors = zip [0..] fs

instance Challenge [Floor] where
  parse = map parseI
  partOne = show . foldl (flip ($)) 0
  partTwo = show . partTwoI
