module Year2019.Day13 where
import Challenge
import qualified Intcode as IC
import qualified Data.Map as Map
import Data.List.Split (chunksOf)
import Control.Monad.State

data Tile = Empty | Wall | Block | Paddle | Ball deriving (Eq)

toTile :: Int -> Tile
toTile 0 = Empty
toTile 1 = Wall
toTile 2 = Block
toTile 3 = Paddle
toTile 4 = Ball
toTile t = error ("Unrecognized tile code: " ++ show t)

type Screen = Map.Map (Int, Int) Tile

interpretOutput :: [Int] -> Screen
interpretOutput = foldr drawScreen Map.empty . chunksOf 3
  where
    drawScreen :: [Int] -> Screen -> Screen
    drawScreen [dl, dt, tid] screen = Map.insert (dl, dt) (toTile tid) screen
    drawScreen o             _      = error ("Incomplete output: " ++ show o)

partOneI :: IC.Program -> Int
partOneI = length . filter (== Block) . Map.elems . interpretOutput . IC.executeCode []

instance Challenge IC.Program where
  parse = IC.fromString
  partOne = show . partOneI


