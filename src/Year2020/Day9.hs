module Year2020.Day9 where

import Challenge
import Utils (readInt)

import qualified Data.Set as S
import Data.List (tails)
import Control.Monad.State.Lazy
import Control.Applicative (empty)

sums :: [Int] -> S.Set Int
sums is = S.fromList [a + b | a <- is, b <- is]

data CheckState = CheckState { _sums :: S.Set Int, _ns :: [Int] }

recompute :: Int -> State CheckState ()
recompute i = do
  (CheckState _ ns) <- get
  let
    newNs = drop 1 ns ++ [i]
    newSs = sums newNs
  put (CheckState newSs newNs)

check :: [Int] -> State CheckState Int
check (i:is) = do
  (CheckState ss _) <- get
  if i `S.member` ss
    then recompute i >> check is
    else return i

pt1 :: [Int] -> Int
pt1 is = evalState (check ns) (CheckState (sums preamble) preamble)
  where
    preamble = take 25 is
    ns = drop 25 is

sumsTo :: Int -> [Int] -> Maybe (Int, Int)
sumsTo target (a:b:rest) = go (min a b, max a b, a + b) rest
  where
    go _             []     = Nothing
    go (mi, ma, acc) (i:is)
      | acc == target = Just (mi, ma)
      | acc > target = Nothing
      | otherwise = go (min mi i, max ma i, acc + i) is 

contiguousSumsTo :: Int -> [Int] -> Int
contiguousSumsTo target is = head $ do
  ts <- tails is
  case sumsTo target ts of
    Just (mi, ma) -> pure $ mi + ma
    Nothing -> empty

result :: [Int] -> Int
result is = maximum is + minimum is

instance Challenge [Int] where
  parse = map readInt . lines
  partOne = show . pt1
  partTwo = show . contiguousSumsTo 50047984