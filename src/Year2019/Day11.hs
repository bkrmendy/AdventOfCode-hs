module Year2019.Day11 where
import Prelude hiding (Left, Right)
import Challenge
import Control.Monad.State
import qualified Data.Map as Map
import qualified Intcode as IC

data Direction = Direction { _drow :: Int, _dcol :: Int} deriving Show

data TurnCommand = TurnLeft | TurnRight
toTurnCommand :: Int -> TurnCommand
toTurnCommand 0 = TurnLeft
toTurnCommand 1 = TurnRight
toTurnCommand d = error ("Cannot turn on direction " ++ show d)

turn :: TurnCommand -> Direction -> Direction
turn TurnLeft  (Direction drow dcol) = Direction (-dcol) drow
turn TurnRight (Direction drow dcol) = Direction dcol (-drow)

data Color = Black | White
toColor :: Int -> Color
toColor 0 = Black
toColor 1 = White
toColor c = error ("Unrecognized color: " ++ show c)

fromColor :: Color -> Int
fromColor Black = 0
fromColor White = 1

type Grid = Map.Map (Int, Int) Color

data WalkState = WalkState { _grid :: Grid
                           , _position :: (Int, Int)
                           , _direction :: Direction }

-- update grid with new color and position
-- change direction
-- move forward

paint :: Color -> WalkState -> WalkState
paint color (WalkState grid pos dir) = WalkState grid2 pos dir
  where grid2 = Map.insert pos color grid

chDir :: TurnCommand -> WalkState -> WalkState
chDir turnDir (WalkState grid pos dir) = WalkState grid pos (turn turnDir dir)

proceed :: WalkState -> WalkState
proceed (WalkState g (row, col) d@(Direction drow dcol)) = WalkState g (row + drow, col + dcol) d

step :: TurnCommand -> Color -> WalkState -> WalkState
step dir color = proceed . chDir dir . paint color

walk :: WalkState -> State IC.ProcessState Grid
walk walkState@(WalkState grid position _) = do
  IC.addProcessInputs [fromColor $ Map.findWithDefault White position grid]
  out <- IC.continueExecution
  halted <- IC.hasShutDown
  if halted
    then return grid
    else case out of
      [color, dir] -> walk (step (toTurnCommand dir) (toColor color) walkState)
      _ -> error "incomplete output"

partOneI :: IC.Program -> Grid
partOneI program = evalState (walk startingState) (IC.initializeProcess program [])
  where
    startingState = WalkState Map.empty (0, 0) (Direction { _drow = -1, _dcol = 0 })

instance Challenge IC.Program where
  parse = IC.fromString
  partOne = show . Map.size . partOneI
  partTwo = show . length . IC.unProgram
