module Year2021.Day13 where
  
import Challenge
import Utils (readInt)

import Data.List (foldl')
import Data.List.Split (splitOn)
import qualified Data.Set as S

data Dot = MkDot { _x :: Int, _y :: Int } deriving (Eq, Ord)  
data Fold = MkFold { _axis :: String, _offset :: Int }

mkDot :: String -> Dot
mkDot input = let [x, y] = splitOn "," input in MkDot (readInt x) (readInt y)

mkFold :: String -> Fold
mkFold input = MkFold a (readInt o)
  where [_, _, f] = words input
        [a, o] = splitOn "=" f

type Manual = ([Dot], [Fold])
 
parseI :: String -> Manual
parseI input = (dots, folds)
  where [ds, fs] = splitOn "\n\n" input
        dots = mkDot <$> lines ds
        folds = mkFold <$> lines fs
        
foldX, foldY :: Int -> Dot -> Dot
foldX offset dot@(MkDot x y) | x <= offset = dot
                             | otherwise = MkDot (2 * offset - x) y
foldY offset dot@(MkDot x y) | y <= offset = dot
                             | otherwise = MkDot x (2 * offset - y)
        
foldAlong :: Fold -> Dot -> Dot
foldAlong (MkFold axis offset) dot = case axis of
  "x" -> foldX offset dot
  "y" -> foldY offset dot

doFolds :: S.Set Dot -> [Fold] -> S.Set Dot
doFolds = foldl' (\dots f -> S.map (foldAlong f) dots) 

partOneI :: Manual -> Int
partOneI (dots, folds) = S.size $ S.map (foldAlong $ head folds) (S.fromList dots)

bounds :: (Ord a) => [a] -> (a, a)
bounds as = (minimum as, maximum as)

visualize :: S.Set Dot -> [String]
visualize dots = [[pixel (MkDot x y) | x <- [minX .. maxX]] | y <- [minY .. maxY] ]
  where pixel c = if c `S.member` dots then '#' else ' '
        ds = S.elems dots
        (minX, maxX) = bounds $ map _x ds
        (minY, maxY) = bounds $ map _y ds

partTwoI :: Manual -> String
partTwoI (dots, folds) = unlines $ visualize $ doFolds (S.fromList dots) folds 

instance Challenge Manual where
  parse = parseI
  partOne = show . partOneI
  partTwo = partTwoI
  






