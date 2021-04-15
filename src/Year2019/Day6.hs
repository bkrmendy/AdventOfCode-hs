module Year2019.Day6 where
import Challenge
import Data.List.Split (splitOn)
import qualified Data.Map as Map

type Atlas = Map.Map String [String]

fromTuples :: [(String, String)] -> Atlas
fromTuples = foldr update Map.empty
  where
    update (orb, planet) atlas = Map.insert orb (planet:(Map.findWithDefault [] orb atlas)) atlas

orbits :: Int -> String -> Atlas -> Int
orbits steps from atlas = steps + sum [orbits (steps + 1) o atlas | o <- thisOrbits]
  where
    thisOrbits = Map.findWithDefault [] from atlas

path :: String -> String -> Atlas -> [[String]]
path from to atlas
  | from == to = [[to]]
  | otherwise = [from:branch | orb <- here
                             , branch <- path orb to atlas]
  where
     here = Map.findWithDefault [] from atlas


pathDiff :: Atlas -> Int
pathDiff atlas = (length san - length same) + (length you - length same)
  where
    you = head (path "COM" "YOU" atlas)
    san = head (path "COM" "SAN" atlas)
    same = takeWhile (uncurry (==)) (zip you san)

instance Challenge Atlas where
  parse = fromTuples . map ((\[a, b] -> (a, b)) . splitOn ")") . splitOn "\n"
  partOne = show . orbits 0 "COM"
  partTwo = show . (\p -> p - 2) . pathDiff

