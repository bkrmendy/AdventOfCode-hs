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

toAscii :: String -> [Int]
toAscii = map ord

program :: [String]
program = [  
          ]

instance Challenge IC.Program where
  parse = IC.fromString
  partOne = show . sum . map (uncurry (*)) . crossings . makeMap
  partTwo = unlines . makeMap