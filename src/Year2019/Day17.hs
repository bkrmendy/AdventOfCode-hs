{-# LANGUAGE TupleSections #-}
module Year2019.Day17 where
import            Challenge
import            Utils (replace)
import qualified  Intcode as IC

import qualified  Data.Array as A
import            Data.Char (chr, ord)
import            Data.Foldable (asum)
import            Data.List (group, find, inits, unfoldr, stripPrefix, intercalate)
import            Data.List.Split (chunksOf, splitOn)
import            Data.Maybe (mapMaybe, fromJust, catMaybes, listToMaybe)
import            Control.Applicative (empty)
import            Control.Monad (guard)
import            Control.Monad.State

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

data Step = L | F | R deriving (Eq, Show)
data LookAhead = Clear | TurnLeft | TurnRight | Edge
data Robot = Robot { _position :: (Int, Int), _direction :: (Int, Int) }

edge :: Maybe LookAhead
edge = Just Edge

under :: Grid -> Robot -> Maybe Char
under grid (Robot pos _)
  | A.inRange (A.bounds grid) pos = Just $ grid A.! pos
  | otherwise = Nothing

forward :: Robot -> Grid -> Maybe LookAhead
forward robot grid = do
  tile <- under grid (proceed robot)
  if tile == '#'
    then return Clear
    else empty

left :: Robot -> Grid -> Maybe LookAhead
left robot grid = do
  tile <- under grid (proceed . turnLeft $ robot)
  if tile == '#'
   then return TurnLeft
   else empty

right :: Robot -> Grid -> Maybe LookAhead
right robot grid = do
  tile <- under grid (proceed . turnRight $ robot)
  if tile == '#'
   then return TurnRight
   else empty

lookahead :: Robot -> Grid -> LookAhead
lookahead robot grid = head $ catMaybes [forward robot grid, left robot grid, right robot grid, edge]

turnLeft, turnRight :: Robot -> Robot
turnLeft (Robot pos (dr, dc)) = Robot pos (-dc, dr)
turnRight (Robot pos (dr, dc)) = Robot pos (dc, -dr)

proceed :: Robot -> Robot
proceed (Robot (r, c) (dr, dc)) = Robot (r + dr, c + dc) (dr, dc)

step :: Robot -> Grid -> [Step]
step robot grid =
  case lookahead robot grid of
    Clear -> F : step (proceed robot) grid
    TurnLeft -> L : F : step (proceed . turnLeft $ robot) grid
    TurnRight -> R : F : step (proceed . turnRight $ robot) grid
    Edge -> []

walk :: Grid -> [Step]
walk grid = step (Robot pos (-1, 0)) grid
  where pos = fst . fromJust . find ((== '^') . snd) $ A.assocs grid

-- | From here on, copied from
-- https://github.com/mstksg/advent-of-code-2019/blob/df2b1c76ad26ad20306f705e923a09b14d538374/src/AOC/Challenge/Day17.hs#L97
type CompressedStep = Either Int Int

showCS :: CompressedStep -> String
showCS (Left i) = "L," ++ show i
showCS (Right i) = "R," ++ show i

compact :: [Step] -> [CompressedStep]
compact = mapMaybe go . chunksOf 2 . group
  where
    go [[L], steps] = Just $ Left (length steps)
    go [[R], steps] = Just $ Right (length steps)
    go _            = Nothing

findProgs :: Eq a => [a] -> Maybe ([a], [a], [a])
findProgs as = listToMaybe $ do
  a <- validPrefix as

  let withoutA = neSplitOn a as
  b <- case withoutA of
    [] -> empty
    (bs:_) -> validPrefix bs

  let withoutB = neSplitOn b =<< withoutA
  c <- case withoutB of
    [] -> empty
    (cs:_) -> validPrefix cs

  let withoutC = neSplitOn c =<< withoutB
  guard $ null withoutC

  pure (a, b, c)

  where
    validPrefix = take 5 . filter (not . null) . inits
    neSplitOn x = filter (not . null) . splitOn x

data Routine = A | B | C deriving (Show)

chomp :: Eq a => [([a], b)] -> [a] -> [b]
chomp progs = unfoldr go
  where
    go xs = asum
      [ (r,) <$> stripPrefix prog xs
      | (prog, r) <- progs
      ]

makeProgram :: Grid -> (String, String, String, String)
makeProgram grid =
  let
    path = (compact . walk) grid
    Just (a, b, c) = findProgs path
  in (
    intercalate "," $ chomp [(a,"A"),(b,"B"),(c,"C")] path,
    intercalate "," $ showCS <$> a,
    intercalate "," $ showCS <$> b,
    intercalate "," $ showCS <$> c
    )

setMode :: Int -> IC.Program -> IC.Program
setMode i (IC.Program program) = IC.Program $ replace 0 i program

doState :: s -> State s c -> c
doState = flip evalState

toAscii :: String -> [Int]
toAscii = map ord

pt2 :: IC.Program -> Int
pt2 program =
  let grid = mkGrid program
      (r, a, b, c) = makeProgram grid
      moveProg = setMode 2 program
  in doState (IC.initializeProcess moveProg []) $ do
    IC.addProcessInputs (toAscii r ++ [10] ++ toAscii a ++ [10] ++ toAscii b ++ [10] ++ toAscii c ++ [10] ++ [ord 'n'] ++ [10])
    last <$> IC.continueExecution

instance Challenge IC.Program where
  parse = IC.fromString
  partOne = show . sum . map crossingWeight . windows . mkGrid
  partTwo = show . pt2