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

colorToInt :: Color -> Int
colorToInt Black = 0
colorToInt White = 1

colorToChar :: Color -> Char
colorToChar Black = ' '
colorToChar White = '#'

type Grid = Map.Map (Int, Int) Color

data WalkState = WalkState { _grid :: Grid
                           , _position :: (Int, Int)
                           , _direction :: Direction
                           }

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
  IC.addProcessInputs [colorToInt $ Map.findWithDefault Black position grid]
  out <- IC.continueExecution
  halted <- IC.hasShutDown
  if halted
    then return grid
    else case out of
      [color, dir] -> walk (step (toTurnCommand dir) (toColor color) walkState)
      _ -> error "incomplete output"

makeGrid :: Grid -> IC.Program -> Grid
makeGrid startGrid program = evalState (walk startingState) (IC.initializeProcess program [])
  where
    startingState = WalkState startGrid (0, 0) (Direction { _drow = -1, _dcol = 0 })

drawGrid :: Grid -> String
drawGrid grid = unlines rows
  where
    minRow = minimum $ map fst $ Map.keys grid
    maxRow = maximum $ map fst $ Map.keys grid
    minCol = minimum $ map snd $ Map.keys grid
    maxCol = maximum $ map snd $ Map.keys grid
    rows = [[colorToChar (Map.findWithDefault Black (row, col) grid) | col <- [minCol..maxCol]] | row <- [minRow..maxRow]]

instance Challenge IC.Program where
  parse = IC.fromString
  partOne = show . Map.size . makeGrid (Map.singleton (0, 0) Black)
  partTwo = drawGrid . makeGrid (Map.singleton (0, 0) White) -- (Björk, Lopez Jennfier, Uther Pendragon)
