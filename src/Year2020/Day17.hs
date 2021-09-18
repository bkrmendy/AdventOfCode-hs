module Year2020.Day17 where
  
import Challenge

import Control.Monad (guard)
import Control.Applicative (empty)
import qualified Data.Set as S
  
data State a = Active a | Inactive a

data Dim2 = Dim2 Int Int
data Dim3 = Dim3 Int Int Int      deriving (Eq, Ord, Show)
data Dim4 = Dim4 Int Int Int Int  deriving (Eq, Ord)

class (Eq a, Ord a) => PocketDimension a where
  spruce :: Dim2 -> a
  neighbors :: a -> [a]
  
type ConwayCube a = S.Set a
  
instance PocketDimension Dim3 where
  spruce (Dim2 x y) = Dim3 x y 0
  neighbors (Dim3 x y z) = [Dim3 xx yy zz | xx <- [x - 1 .. x + 1]
                                          , yy <- [y - 1 .. y + 1]
                                          , zz <- [z - 1 .. z + 1]
                                          , (xx, yy, zz) /= (x, y, z)]

instance PocketDimension Dim4 where
  spruce (Dim2 x y) = Dim4 x y 0 0
  neighbors (Dim4 x y z w) = [Dim4 xx yy zz ww | xx <- [x - 1 .. x + 1]
                                               , yy <- [y - 1 .. y + 1]
                                               , zz <- [z - 1 .. z + 1]
                                               , ww <- [w - 1 .. w + 1] 
                                               , (xx, yy, zz, ww) /= (x, y, z, w)]


transition :: [a] -> State a -> Maybe a
transition ns (Active p) | length ns == 3 || length ns == 2 = Just p
                         | otherwise = Nothing
transition ns (Inactive p) | length ns == 3 = Just p
                           | otherwise = Nothing
                           

status :: (PocketDimension a) => a -> ConwayCube a -> State a
status p cube | S.member p cube = Active p
              | otherwise = Inactive p
  
nextState :: (PocketDimension a) => a -> ConwayCube a -> Maybe a
nextState p cube = transition realNeighbors (status p cube)
  where realNeighbors = filter (`S.member` cube) (neighbors p)   
  
step :: (PocketDimension a) => ConwayCube a -> ConwayCube a
step cube = S.fromList $ do
  p <- S.elems cube
  point <- p:neighbors p
  case nextState point cube of
    Just po -> pure po
    Nothing -> empty
  
cube3d :: [Dim2] -> ConwayCube Dim3
cube3d = S.fromList . map (\p -> spruce p :: Dim3)

cube4d :: [Dim2] -> ConwayCube Dim4
cube4d = S.fromList . map (\p -> spruce p :: Dim4)
  
mkCube :: String -> [Dim2]
mkCube input = do
  (iRow, line) <- zip [0..] (lines input)
  (iColumn, point) <- zip [0..] line
  guard (point == '#')
  pure (Dim2 iColumn iRow)
  
solve :: (PocketDimension a) => ([Dim2] -> ConwayCube a) -> [Dim2] -> Int
solve make = S.size . step6 . make
  where step6 = step . step . step . step . step . step 

instance Challenge [Dim2] where
  parse = mkCube
  partOne = show . solve cube3d
  partTwo = show . solve cube4d
  