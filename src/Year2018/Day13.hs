module Year2018.Day13 where
  
import Data.List (cycle)
import qualified Data.Array.Unboxed as A

type Map = A.Array (Int, Int) Char
  
data Position = MkPosition { _row :: Int, _column :: Int } deriving (Eq)
data Direction = MkDirection { _up :: Int, _left :: Int }
type Turn = Direction -> Direction

data Cart = MkCart
  { _position :: Position
  , _direction :: Direction
  , _steps :: [Turn]
  }
  
type Challenge = (Map, [Cart])

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
  
mkMap :: String -> (Map, [Cart])
mkMap = undefined

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
        
simulate :: Map -> [Cart] -> Int -> Position
simulate field carts turn = case collision carts of
  Just po -> po
  Nothing -> simulate field (map (step field) carts) (turn + 1)
  

        
        
        
  








 