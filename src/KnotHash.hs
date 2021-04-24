module KnotHash (
    hash
  , knotHash
  , mkKnot
  , HashState(HashState, _hash, _currentPos, _skipSize)
) where

import            Numeric (showHex)
import            Data.Char (ord)
import            Data.Bits (xor)
import            Data.List (intercalate)
import            Data.List.Split (chunksOf)
import qualified  Data.Array as A
import            Control.Monad.State

data HashState = HashState { _currentPos :: Int, _skipSize :: Int, _hash :: A.Array Int Int } deriving Show

mkKnot :: [(Int, Int)] -> A.Array Int Int
mkKnot is = A.array (0, length is - 1) is

revert :: Int -> Int -> A.Array Int Int -> A.Array Int Int
revert from len knot = mkKnot (revs <> straights)
  where
    (_, top) = A.bounds knot
    revs = [(ic, knot A.! (i `rem` (top + 1))) | i <- [from..from+(len - 1)]
                                                , let ic = (from + from + len - 1 - i) `rem` (top + 1)]
    straights = [(ic, knot A.! ic) | i <- [from+len..from+top], let ic = i `rem` (top + 1)]

hash :: [Int] -> State HashState ()
hash is = do
  forM_ is $ \i -> do
    (HashState cp ss h) <- get
    nextHash <- pure $ revert cp i h
    modify' $ \s -> s { _currentPos = cp + ss + i, _skipSize = ss + 1, _hash = nextHash }

runHash :: [Int] -> State HashState ()
runHash is = do
  forM_ [1..64] $ \_ -> hash is
  
dense :: [Int] -> [Int]
dense is = map mix (chunksOf 16 is)
  where mix = \xs -> foldr xor (head xs) (tail xs)

toHex :: [Int] -> String
toHex = concatMap showHexLeading
  where
    showHexLeading i
      | i <= 15    = "0" <> showHex i ""
      | otherwise = showHex i ""

tweak :: String -> [Int]
tweak is = map ord is ++ [17, 31, 73, 47, 23]

knotHash :: String -> String
knotHash input = toHex $ dense . A.elems . _hash $ execState (runHash (tweak input)) (HashState 0 0 $ mkKnot (zip [0..] [0..255]))

-- 473fa9d8ab24631f10c3e20d9b849d0