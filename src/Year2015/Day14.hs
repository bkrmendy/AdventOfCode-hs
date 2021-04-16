module Year2015.Day14 where
import Challenge
import Utils (int, parseLines, maxIndex, updateAtIndex)
import Text.Parsec hiding (State)
import Control.Monad.State

data Reindeer = Reindeer { _speed :: Int, _active :: Int, _rest :: Int } deriving (Show)

fluff :: Parsec String () String
fluff = many1 (letter <|> char ' ' <|> char '/' <|> char '.' <|> char ',')

reindeer :: Parsec String () Reindeer
reindeer = Reindeer <$> (fluff *> int) <*> (fluff *> int) <*> (fluff *> int <* fluff)

bursts :: Reindeer -> [Int]
bursts rein@(Reindeer s a r) = replicate a s ++ replicate r 0 ++ bursts rein

add :: [Int] -> [Int] -> [Int]
add = zipWith (+)

newtype Race = Race { _distances_points :: [(Int, Int)] }

race :: Int -> [[Int]] -> State Race Race
race (-1) _  = get
race time is = do
  (Race dps) <- get
  newDistances <- pure $ map head is `add` (map fst dps)
  lead <- pure $ maximum (map fst dps)
  newPoints <- pure $ map (\(d, p) -> if d == lead then p + 1 else p) dps
  put (Race $ zip newDistances newPoints)
  race (time - 1) (map tail is)

runRace :: Int -> [[Int]] -> Race
runRace time reindeers = evalState (race time reindeers) (Race zs)
  where zs = replicate (length reindeers) (0, 0)

judge :: Int -> (Race -> [Int]) -> [[Int]] -> Int
judge time f = maximum . f . runRace time

instance Challenge [[Int]] where
  parse = map bursts . parseLines (sepBy1 reindeer newline)
  partOne = show . judge 2503 (map fst . _distances_points)
  partTwo = show . judge 2503 (map snd . _distances_points)
