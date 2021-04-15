module Year2019.Day12 where
import Prelude hiding (fst, snd)
import Challenge
import Utils
import Text.Parsec
import Data.Hashable
import Data.Bits (xor)
import qualified Data.HashSet as HashSet

absSum :: (Int, Int, Int) -> Int
absSum (a, b, c) = abs a + abs b + abs c

triAdd :: (Int, Int, Int) -> (Int, Int, Int) -> (Int, Int, Int)
triAdd (a, b, c) (x, y, z) = (a + x, b + y, c + z)

data Moon = Moon { _position :: (Int, Int, Int), _velocity :: (Int, Int, Int) } deriving (Show, Eq, Ord)
data Moon1D = Moon1D { _position1D :: Int, _velocity1D :: Int } deriving (Eq, Ord)

instance Hashable Moon1D where
  hashWithSalt salt (Moon1D pos vel) = hashWithSalt salt pos `xor` hashWithSalt salt vel

type Projection = Moon -> Moon1D

project :: ((Int, Int, Int) -> Int) -> Projection
project f (Moon pos vel) = Moon1D (f pos) (f vel)

parseMoon :: Parsec String () (Int, Int, Int)
parseMoon = (,,) <$> (string "<x=" *> int) <*> (string ", y=" *> int) <*> (string ", z=" *> int <* string ">")

energy :: Moon -> Int
energy (Moon position velocity) = potential * kinetic
  where
    potential = absSum position
    kinetic   = absSum velocity

step :: [Moon] -> [Moon]
step moons = map velocity moons
  where
    gravity :: Moon -> (Int, Int, Int)
    gravity (Moon (x, y, z) _) = foldr triAdd (0, 0, 0) [ (sgn (a - x), sgn (b - y), sgn (c - z)) | (Moon (a, b, c) _) <- moons]
    velocity :: Moon -> Moon
    velocity moon@(Moon pos vel) =
      let vel' = vel `triAdd` gravity moon
      in  Moon (pos `triAdd` vel') vel'

simulateSteps :: Int -> [Moon] -> [Moon]
simulateSteps n moons
  | n == 0 = moons
  | otherwise = simulateSteps (n - 1) (step moons)

simulate1D :: Projection -> HashSet.HashSet [Moon1D] -> [Moon] -> Int
simulate1D p cache ms
  | HashSet.member pms cache = HashSet.size cache
  | otherwise = simulate1D p (HashSet.insert pms cache) (step ms)
    where
      pms = map p ms

simulateSeen :: [Moon] -> Int
simulateSeen moons = x `lcm` y `lcm` z
  where
    x = simulate1D (project fst) HashSet.empty moons
    y = simulate1D (project snd) HashSet.empty moons
    z = simulate1D (project thd) HashSet.empty moons

instance Challenge [Moon] where
  parse = map (\p -> Moon p (0, 0, 0)) . parseLines (sepBy1 parseMoon newline)
  partOne = show . foldr ((+) . energy) 0 . simulateSteps 1000
  partTwo = show . simulateSeen
