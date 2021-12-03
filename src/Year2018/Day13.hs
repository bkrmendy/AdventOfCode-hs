module Year2018.Day13 where
  
import Challenge
import Data.List (cycle, foldl')
import qualified Data.Array.Unboxed as A

type Map = A.Array (Int, Int) Char
  
data Position = MkPosition { _row :: Int, _column :: Int } deriving (Eq)

instance Show Position where
  show (MkPosition row column) = show column ++ "," ++ show row

data Direction = MkDirection { _up :: Int, _left :: Int }
type Turn = Direction -> Direction

data Cart = MkCart
  { _position :: Position
  , _direction :: Direction
  , _steps :: [Turn]
  }
  
turnLeft, turnRight, goStraight :: Turn
turnLeft (MkDirection up left) = MkDirection (-left) up
turnRight (MkDirection up left) = MkDirection left (-up)
goStraight = id

advance :: Direction -> Position -> Position
advance (MkDirection up left) (MkPosition row column) = MkPosition (row + up) (column + left) 

cart :: Position -> Char -> Maybe Cart  
cart position dir = case dir of
  '>' -> Just $ MkCart position (MkDirection 0 -1) steps
  '<' -> Just $ MkCart position (MkDirection 0 1) steps
  '^' -> Just $ MkCart position (MkDirection 1 0) steps
  'v' -> Just $ MkCart position (MkDirection -1 0) steps
  _   -> Nothing
  where steps = cycle [turnLeft, goStraight, turnRight]
  
replace :: Char -> Char
replace '>' = '-'
replace '<' = '-'
replace '^' = '|'
replace 'v' = '|'
replace  c  = c
  
collection :: [(((Int, Int), Char), [Cart])] -> (([((Int, Int), Char)], [Cart])
collection = foldl' go ([], [])
  where go :: ([((Int, Int), Char)], [Cart]) -> (((Int, Int), Char), [Cart]) -> ([((Int, Int), Char)], [Cart])
        go (maps, carts) (mapThing, []) = (mapThing:maps, carts)
        go (maps, carts) (mapThing, [c]) = (mapThing:maps, c:carts)
  
mkMap :: String -> (Map, [Cart])
mkMap input = (A.array (nRows, nColumns) maps, carts)
  where rows = lines input
        (nRows, nColumns) = (length rows, length (head rows))
        (maps, carts) = collection $ do
          (row, rowIdx) <- zip rows [0..]
          (column, columnIdx) <- zip row [0..]
          pure ((rowIdx, columnIdx), replace column)

collision :: [Cart] -> Maybe Position
collision carts = case collisions of
  [] -> Nothing
  (c:_) -> Just c
  where collisions = [_position c1 | c1 <- carts, c2 <- carts, _position c1 == _position c2]
  
nextDirection :: Char -> [Turn] -> Direction -> ([Turn], Direction)
nextDirection '+'  (turn:rest) direction = (rest, turn direction)
nextDirection '\\' turns       direction = (turns, turnLeft direction)
nextDirection '/'  turns       direction = (turns, turnRight direction)
nextDirection _    turns       direction = (turns, direction)
  
step :: Map -> Cart -> Cart
step field (MkCart pos@(MkPosition row col) direction turns) = MkCart (advance newDirection pos) newDirection newTurns 
  where tile = field A.! (row, col)
        (newTurns, newDirection) = nextDirection tile turns direction 
        
simulate :: Map -> [Cart] -> Position
simulate field carts = case collision carts of
  Just po -> po
  Nothing -> simulate field (map (step field) carts) (turn + 1)
  
instance Challenge (Map, [Cart]) where
  parse = mkMap
  partOne = show . uncurry simulate
        
        
  








 