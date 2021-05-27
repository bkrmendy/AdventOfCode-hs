module Year2019.Day17 where
import            Challenge
import qualified  Intcode as IC

import qualified  Data.Array as A
import            Data.Char (chr)
import            Data.List (group, find)
import            Data.List.Split (chunksOf)
import            Data.Maybe (mapMaybe, fromJust, catMaybes)

crossing :: [Char]
crossing = concat [
           ".#."
         , "###"
         , ".#."
         ]

type Grid = A.Array (Int, Int) Char

data Window = Window { _center :: (Int, Int), _data :: [Char] }

crossingWeight :: Window -> Int
crossingWeight (Window (r, c) win) = if win == crossing then r * c else 0

window :: Grid -> (Int, Int) -> Window
window grid (r, c) = Window (r, c) [ grid A.! (ra, ca) | ra <- [r - 1..r + 1], ca <- [ c - 1..c + 1]]

windows :: Grid -> [Window]
windows grid = [window grid (r, c) | c <- [1..width - 1], r <- [1..height - 1]]
  where (_, (height, width)) = A.bounds grid

stringToGrid :: [String] -> Grid
stringToGrid raw = A.array ((0, 0), (height, width)) [((r, c), get r c) | r <- [0..height], c <- [0..width]]
  where
    (width, height) = (length (head raw) - 1, length raw - 1)
    get = \row col -> (raw !! row) !! col

mkGrid :: IC.Program -> Grid
mkGrid = stringToGrid . filter (not . null) . lines . map chr . IC.executeCode []


data Step = L | F | R deriving (Eq)
data LookAhead = Clear | TurnLeft | TurnRight | Edge
data Robot = Robot { _position :: (Int, Int), _direction :: (Int, Int) }

edge :: Maybe LookAhead
edge = Just Edge

lookupGrid :: Grid -> (Int, Int) -> Maybe Char
lookupGrid grid coords
  | A.inRange (A.bounds grid) coords = Just $ grid A.! coords
  | otherwise = Nothing

forward :: Robot -> Grid -> Maybe LookAhead
forward = undefined

left :: Robot -> Grid -> Maybe LookAhead
left = undefined

right :: Robot -> Grid -> Maybe LookAhead
right = undefined

lookahead :: Robot -> Grid -> LookAhead
lookahead robot grid = head $ catMaybes [forward robot grid, left robot grid, right robot grid, edge]

turnLeft, turnRight :: (Int, Int) -> (Int, Int)
turnLeft (dr, dc) = (dc, -dr)
turnRight (dr, dc) = (-dc, dr)

proceed :: Robot -> Robot
proceed (Robot (r, c) (dr, dc)) = Robot (r + dr, c + dc) (dr, dc)

step :: Robot -> Grid -> [Step]
step robot@(Robot pos dir) grid =
  case lookahead robot grid of
    Clear -> F : step (Robot (move pos dir) dir) grid
    TurnLeft -> L : step (Robot (move pos dir) (turnLeft dir)) grid
    TurnRight -> R : step (Robot (move pos dir) (turnRight dir)) grid
    Edge -> []
  where
    move (r, c) (dr, dc) = (r + dr, c + dc)

walk :: Grid -> [Step]
walk grid = step (Robot pos (1, 0)) grid
  where pos = fst . fromJust . find ((== '^') . snd) $ A.assocs grid
       

compact :: [Step] -> [Either Int Int]
compact = mapMaybe go . chunksOf 2 . group
  where
    go [[L], steps] = Just $ Left (length steps)
    go [[R], steps] = Just $ Right (length steps)
    go _            = Nothing

instance Challenge IC.Program where
  parse = IC.fromString
  partOne = show . sum . map crossingWeight . windows . mkGrid
  -- part 2 based on: https://github.com/mstksg/advent-of-code-2019/blob/master/reflections.md
  partTwo = show . compact . walk . mkGrid