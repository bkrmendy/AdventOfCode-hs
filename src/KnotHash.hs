module KnotHash (
    hash
  , mkKnot
  , HashState(HashState, _hash, _currentPos, _skipSize)
) where

import qualified  Data.Array as A
import            Control.Monad.State

data HashState = HashState { _currentPos :: Int, _skipSize :: Int, _hash :: A.Array Int Int } deriving Show

mkKnot :: [(Int, Int)] -> A.Array Int Int
mkKnot is = A.array (0, length is - 1) is

revert :: Int -> Int -> A.Array Int Int -> A.Array Int Int
revert from len knot = mkKnot (revs <> straights)
  where
    revs = [(ic, knot A.! (i `rem` 256)) | i <- [from..from+(len - 1)], let ic = (from + from + len - 1 - i) `rem` 256]
    straights = [(ic, knot A.! ic) | i <- [from+len..from+255], let ic = i `rem` 256]

hash :: [Int] -> State HashState ()
hash is = do
  forM_ is $ \i -> do
    (HashState cp ss h) <- get
    nextHash <- pure $ revert cp i h
    modify' $ \s -> s { _currentPos = cp + ss + i, _skipSize = ss + 1, _hash = nextHash }