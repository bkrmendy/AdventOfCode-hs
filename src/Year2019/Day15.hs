module Year2019.Day15 where
import Challenge
import Data.Maybe (catMaybes)
import qualified Intcode as IC
import qualified Data.Map as Map
import Control.Monad.State

type Position = (Int, Int)
data Droid = Droid { _position :: Position, _state :: IC.ProcessState, _steps :: Int }

type Grid = Map.Map Position Int

move :: Position -> Int -> Position
move (row, col) 1 = (row - 1, col)
move (row, col) 2 = (row + 1, col)
move (row, col) 3 = (row, col - 1)
move (row, col) 4 = (row, col + 1)
move _          _ = undefined

step :: Droid -> Grid -> Int -> Maybe (Int, Droid)
step (Droid pos sta ste) seen direction
  | Map.member newPos seen || out == 0 = Nothing
  | otherwise = Just (out, Droid newPos newState (ste + 1))
  where
    newPos = move pos direction
    (out:_, newState) = runState (do { IC.addProcessInputs [direction]; IC.continueExecution }) sta

search :: (Int, Grid) -> [Droid] -> (Int, Grid)
search (shortest, grid) [] = (shortest, grid)
search (shortest, grid) (droid:rest)
  | any ((== 2) . fst) newDroids = search (min shortest $ _steps droid + 1, newGrid) (rest ++ aliveDroids)
  | otherwise = search (shortest, newGrid) (rest ++ aliveDroids)
  where
    newDroids = catMaybes [step droid grid direction | direction <- [1..4]]
    newGrid = foldr (\(t, d) -> Map.insert (_position d) t) grid newDroids
    aliveDroids = map snd (filter ((/= 0) . fst) newDroids)

oxyPosition :: Grid -> Position
oxyPosition grid = head [pos | (pos, 2) <- Map.assocs grid]

floodFill :: Grid -> Map.Map Position Int -> [Position] -> Map.Map Position Int
floodFill _ seen [] = seen
floodFill grid seen (pos:rest) = floodFill grid newSeen (rest ++ nextSteps)
  where
    t = seen Map.! pos
    nextSteps = filter (`Map.notMember` seen) $ filter (`Map.member` grid) [move pos dir | dir <- [1..4]]
    newSeen = foldr (\s -> Map.insert s (t + 1)) seen nextSteps

partTwoI :: Grid -> Int
partTwoI grid = lastFilledTilesTime $ floodFill grid (Map.singleton oxyPos 0) [oxyPos]
  where
    oxyPos = oxyPosition grid

searchForOxy :: IC.Program -> (Int, Grid)
searchForOxy program = search (1000, Map.singleton (0, 0) 1) [Droid (0, 0) (IC.initializeProcess program []) 0]

lastFilledTilesTime :: Map.Map Position Int -> Int
lastFilledTilesTime = maximum . Map.elems

instance Challenge IC.Program where
  parse = IC.fromString
  partOne = show . fst . searchForOxy
  partTwo = show . partTwoI . snd . searchForOxy