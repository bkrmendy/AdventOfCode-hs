module Year2020.Day22 where
  
import Challenge
import Utils (readInt)

import Data.List.Split (splitOn)
import qualified Data.Set as S

import Debug.Trace

type Deck = [Int]
type Puzzle = (Deck, Deck)

data Victory = Player Int | Crab Int deriving Show
  
play :: Deck -> Deck -> Victory
play []     crab = Crab $ score crab
play player []   = Player $ score player
play (p:player) (c:crab)
  | p > c     = play (player ++ [p, c]) crab
  | otherwise = play player (crab ++ [c, p])

score :: [Int] -> Int
score = sum . zipWith go [1..] . reverse
  where go c n = c * n
  
playRecursive :: S.Set (Int, Int) -> [Int] -> [Int] -> Victory
playRecursive history player crab
  | null crab = Player scorePlayer
  | null player = Crab scoreCrab
  | S.member (scorePlayer, scoreCrab) history = Player scorePlayer
  | topPlayer <= length restPlayer && topCrab <= length restCrab = case playRecursive S.empty (take topPlayer restPlayer) (take topCrab restCrab) of
    Player _ -> nextRound (restPlayer ++ [topPlayer, topCrab]) restCrab
    Crab _   -> nextRound restPlayer (restCrab ++ [topCrab, topPlayer])
  | topCrab > topPlayer = nextRound restPlayer (restCrab ++ [topCrab, topPlayer])
  | otherwise = nextRound (restPlayer ++ [topPlayer, topCrab]) restCrab
  where scorePlayer = score player
        scoreCrab = score crab
        (topPlayer:restPlayer) = player
        (topCrab:restCrab) = crab
        nextRound p c = playRecursive (S.insert (scorePlayer, scoreCrab) history) p c

parseI :: String -> Puzzle
parseI input = (parseCards a, parseCards b) 
  where (a:b:_) = splitOn [""] $ lines input
        parseCards = map readInt . drop 1
        
instance Challenge Puzzle where
  parse = parseI
  partOne = show . uncurry play
  partTwo = show . uncurry (playRecursive S.empty)