module Year2020.Day17 where
  
import Challenge
import GameOfLife
import Utils (composeN)

import Control.Monad (guard)
import qualified Data.Set as S
  
data Dim2 = Dim2 Int Int
data Dim3 = Dim3 Int Int Int      deriving (Eq, Ord, Show)
data Dim4 = Dim4 Int Int Int Int  deriving (Eq, Ord)

spruce3 :: Dim2 -> Dim3
spruce3 (Dim2 x y) = Dim3 x y 0

spruce4 :: Dim2 -> Dim4
spruce4 (Dim2 x y) = Dim4 x y 0 0

transitionI :: Int -> State a -> Maybe a
transitionI ns (Active p)   | ns == 3 || ns == 2  = Just p
transitionI ns (Inactive p) | ns == 3             = Just p
transitionI _  _                                  = Nothing

instance GameOfLife Dim3 where
  transition = transitionI
  neighbors (Dim3 x y z) = [Dim3 xx yy zz | xx <- [x - 1 .. x + 1]
                                          , yy <- [y - 1 .. y + 1]
                                          , zz <- [z - 1 .. z + 1]
                                          , (xx, yy, zz) /= (x, y, z)]

instance GameOfLife Dim4 where
  transition = transitionI
  neighbors (Dim4 x y z w) = [Dim4 xx yy zz ww | xx <- [x - 1 .. x + 1]
                                               , yy <- [y - 1 .. y + 1]
                                               , zz <- [z - 1 .. z + 1]
                                               , ww <- [w - 1 .. w + 1] 
                                               , (xx, yy, zz, ww) /= (x, y, z, w)]
                             
cube3d :: [Dim2] -> GameOfLifeMap Dim3
cube3d = S.fromList . map spruce3

cube4d :: [Dim2] -> GameOfLifeMap Dim4
cube4d = S.fromList . map spruce4
  
mkCube :: String -> [Dim2]
mkCube input = do
  (iRow, line) <- zip [0..] (lines input)
  (iColumn, point) <- zip [0..] line
  guard (point == '#')
  pure (Dim2 iColumn iRow)
  
solve :: (GameOfLife a) => ([Dim2] -> GameOfLifeMap a) -> [Dim2] -> Int
solve make = S.size . step6 . make
  where step6 = composeN step 6

instance Challenge [Dim2] where
  parse = mkCube
  partOne = show . solve cube3d
  partTwo = show . solve cube4d
  