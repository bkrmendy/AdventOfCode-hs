module Year2018.Day4 where
  
import Utils (int, parseL)
import Challenge

import Text.Parsec as P
import Data.List (sort, sortOn, foldl')
import qualified Data.MultiSet as MS
import qualified Data.Map.Strict as M

import Data.Bifunctor (second)
import qualified Debug.Trace as T

data Event
  = GuardBeginsShift Int
  | FallsAsleep
  | WakesUp
  deriving (Eq, Ord, Show)
  
data TimeStamp = TimeStamp { _y :: Int, _mo :: Int, _d :: Int, _h :: Int, _mi :: Int } deriving (Eq, Ord, Show)

type Entry = (TimeStamp, Event)
type Guards = M.Map Int (MS.MultiSet Int)

guardBeginsShift, fallsAsleep, wakesUp, event :: Parsec String () Event
guardBeginsShift = GuardBeginsShift <$> (string "Guard #" *> int <* string " begins shift")
fallsAsleep = FallsAsleep <$ string "falls asleep"
wakesUp = WakesUp <$ string "wakes up"
event = guardBeginsShift <|> fallsAsleep <|> wakesUp

timestamp :: Parsec String () TimeStamp
timestamp = do
  _ <- char '['
  y <- int
  _ <- char '-'
  m <- int
  _ <- char '-'
  d <- int
  _ <- space
  h <- int
  _ <- char ':'
  mi <- int
  _ <- char ']'
  return $ TimeStamp y m d h mi
  
entry :: Parsec String () Entry
entry = (,) <$> (timestamp <* space) <*> event

parseI :: String -> [Entry]
parseI = sort . parseL entry

putMinutes :: Int -> [Int] -> Guards -> Guards
putMinutes guard minutes gs = case M.lookup guard gs of
  Nothing -> M.insert guard (MS.fromList minutes) gs
  Just ms -> M.insert guard (foldr MS.insert ms minutes) gs

deltaM :: TimeStamp -> TimeStamp -> [Int]
deltaM (TimeStamp _ _ _ _ m) (TimeStamp _ _ _ _ m') = go m
  where go g | g == m' = []
             | otherwise = g:go (g + 1 `rem` 60)

step :: (Int, Guards) -> (Entry, Entry) -> (Int, Guards)
step (guard, gs) transition = case transition of
  ((ts, FallsAsleep), (ts', WakesUp)) -> (guard, putMinutes guard (deltaM ts ts') gs)
  ((_, GuardBeginsShift n), _) -> (n, gs)
  ((_, WakesUp), (_, FallsAsleep))        -> (guard, gs)
  ((_, WakesUp), (_, GuardBeginsShift _)) -> (guard, gs)
 
type Strategy = Guards -> Int
  
solve :: Strategy -> [Entry] -> Int
solve strategy es = strategy finalGuards
  where (_, finalGuards) = foldl' step (0, M.empty) (zip es (tail es))
  
strategy1 :: Strategy
strategy1 gs = number * maxMin
  where (number, ms) = last $ sortOn (MS.size . snd) (M.assocs gs)
        (maxMin, _      ) = last . sortOn snd $ MS.toAscOccurList ms

minutes :: Guards -> [(Int, Int, Int)]
minutes guards = do
  (g, ms) <- M.assocs guards
  (m, f) <- MS.toOccurList ms
  pure (g, m, f)

strategy2 :: Strategy
strategy2 = (\(g, m, _) -> g * m) . last . sortOn (\(_, _, f) -> f) . minutes
        
instance Challenge [Entry] where
  parse = parseI
  partOne = show . solve strategy1
  partTwo = show . solve strategy2