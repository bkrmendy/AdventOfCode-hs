module Year2018.Day18 where
import Challenge
import Utils (countWhere)

import Prelude hiding (lookup)
import qualified  Data.Array as A
import            Data.Maybe (catMaybes)
import qualified  Data.Map.Strict as M
import            Control.Monad (guard)

data Tile
  = OpenGround
  | Trees
  | Lumberyard
  deriving (Ord, Eq)

type Grid = A.Array (Int, Int) Tile

lookup :: Grid -> (Int, Int) -> Maybe Tile
grid `lookup` i
  | A.inRange (A.bounds grid) i = Just $ grid A.! i
  | otherwise = Nothing

transform :: Tile -> [Tile] -> Tile
transform OpenGround  tiles = if countWhere (Trees ==) tiles >= 3       then Trees      else OpenGround
transform Trees       tiles = if countWhere (Lumberyard ==) tiles >= 3  then Lumberyard else Trees
transform Lumberyard  tiles = if lumberyards >= 1 && trees >= 1         then Lumberyard else OpenGround
  where
    lumberyards = countWhere (Lumberyard ==) tiles
    trees = countWhere (Trees ==) tiles

neighbors :: Grid -> (Int, Int) -> [Tile]
neighbors grid (tr, tc) = catMaybes $ do
  drow <- [-1..1]
  dcol <- [-1..1]
  let (r, c) = (tr + drow, tc + dcol)
  guard $ (r, c) /= (tr, tc)
  pure $ lookup grid (r, c)

step :: Grid -> Grid
step grid = A.array (A.bounds grid) $ do
  (c, t) <- A.assocs grid
  let ns = neighbors grid c
  pure (c, transform t ns)

mkTile :: Char -> Tile
mkTile '.' = OpenGround
mkTile '|' = Trees
mkTile '#' = Lumberyard
mkTile  t  = error $ "Unrecognized tile: " ++ show t

mkGrid :: [String] -> Grid
mkGrid str = A.array ((0, 0), (rows, cols)) [((r, c), go r c) | r <- [0..rows], c <- [0..cols]]
  where
     rows = length str - 1
     cols = length (head str) - 1
     go = \row col -> mkTile $ (str !! row) !! col

value :: Grid -> Int
value grid = lumberyards * woodedAcres
  where
    tiles = A.elems grid
    lumberyards = countWhere (Lumberyard ==) tiles
    woodedAcres = countWhere (Trees ==) tiles

runI :: Int -> M.Map Grid Int -> Grid -> Int
runI mins seen grid
  | mins == M.size seen = value grid
  | grid `M.member` seen = fastForward mins grid seen
  | otherwise = runI mins nextSeen nextGrid
  where
    nextGrid = step grid
    nextSeen = M.insert grid (M.size seen) seen

fastForward :: Int -> Grid -> M.Map Grid Int -> Int
fastForward mins grid seen = runI ((mins - loop) `rem` (loop - start)) M.empty grid
  where
    start = seen M.! grid
    loop = M.size seen
    

run :: Int -> Grid -> Int
run mins = runI mins M.empty

instance Challenge Grid where
  parse = mkGrid . lines
  partOne = show . run 10
  partTwo = show . run 1000000000