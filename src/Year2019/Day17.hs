module Year2019.Day17 where
import Challenge
import Utils (threes)
import qualified Intcode as IC
import Data.Char (chr, ord)
import Data.Maybe (catMaybes)
import Data.List.Split (splitOn)

crossing = ( ".#."
           , "###"
           , ".#." )   

scan :: [[Char]] -> Int -> [(Int, Int)]
scan [r1, r2, r3] line = catMaybes $ zipWith isCrossing parts [1..]
  where
    parts = zip3 (threes r1) (threes r2) (threes r3)
    isCrossing :: ([Char], [Char], [Char]) -> Int -> Maybe (Int, Int)
    isCrossing section col
      | section == crossing = Just (line, col)
      | otherwise = Nothing

crossings :: [String] -> [(Int, Int)]
crossings grid = concat $ zipWith scan (threes grid) [1..]

makeMap :: IC.Program -> [String]
makeMap = filter (not . null) . splitOn "\n" . map chr . IC.executeCode []

data Step = F | R | L

data Instr = MoveForward Int | TurnLeft | TurnRight

walk :: [String] -> [Step]
walk = undefined

toInstrs :: [Step] -> [Instr]
toInstrs = go []
  where
    go acc [] = reverse acc
    go acc (L:rest) = go (TurnLeft:acc) rest
    go acc (R:rest) = go (TurnRight:acc) rest
    go (MoveForward n:acc) (_:rest) = go (MoveForward (n+1):acc) rest
    go acc (_:rest) = go (MoveForward 1:acc) rest


-- group???


-- | TODO:
--      - refactor to 2D Array


instance Challenge IC.Program where
  parse = IC.fromString
  partOne = show . sum . map (uncurry (*)) . crossings . makeMap
  partTwo = unlines . makeMap