module Year2019.Day13 where
import Challenge
import Utils (sgn)
import qualified Intcode as IC
import qualified Data.Map as Map
import Data.Maybe (fromMaybe)
import Data.List.Split (chunksOf)
import Debug.Trace
import Control.Monad.State

type Screen = Map.Map (Int, Int) Int

interpretOutput :: [Int] -> Screen
interpretOutput = foldr drawScreen Map.empty . chunksOf 3
  where
    drawScreen :: [Int] -> Screen -> Screen
    drawScreen [dl, dt, tid] screen = Map.insert (dl, dt) tid screen
    drawScreen o             _      = error ("Incomplete output: " ++ show o)

partOneI :: IC.Program -> Int
partOneI = Map.size . Map.filter (== 2) . interpretOutput . IC.executeCode []

score :: Screen -> Int
score screen = screen Map.! (-1, 0)

columnOf :: Screen -> Int -> Maybe Int
columnOf screen object = case xs of
    [] -> Nothing
    (c:_) -> Just c
  where xs = [col | ((col, _), tile) <- Map.assocs screen, tile == object]

play :: Int -> Int -> State IC.ProcessState Int
play ball paddle = do
  outs <- IC.continueExecution
  let screen = interpretOutput outs
      ball2 = fromMaybe ball (columnOf screen 4)
      paddle2 = fromMaybe paddle (columnOf screen 3)
  IC.addProcessInputs [sgn (ball2 - paddle2)]
  -- 3 paddle
  -- 4 ball
  hasShutDown <- IC.hasShutDown
  if hasShutDown then return (score screen)
  else play ball2 paddle2

partTwoI :: IC.Program -> Int
partTwoI (IC.Program program) = evalState (play (-1) (-1)) (IC.initializeProcess hackedProgram [0])
  where
    hackedProgram = IC.Program (2:tail program)

instance Challenge IC.Program where
  parse = IC.fromString
  partOne = show . partOneI
  partTwo = show . partTwoI

