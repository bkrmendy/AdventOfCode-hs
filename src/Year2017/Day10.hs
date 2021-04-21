module Year2017.Day10 where
import            Challenge
import            Utils (readInt)

import            Numeric (showHex)
import            Data.Char (ord, intToDigit)
import            Data.Bits (xor)
import            Data.List.Split (splitOn, chunksOf)
import qualified  Data.Array as A
import            Control.Monad.State

data HashState = HashState { _currentPos :: Int, _skipSize :: Int, _hash :: A.Array Int Int } deriving Show

mkKnot :: [(Int, Int)] -> A.Array Int Int
mkKnot = A.array (0, 255)

revert :: Int -> Int -> A.Array Int Int -> A.Array Int Int
revert from len knot = mkKnot (revs <> straights)
  where
    revs = [(ic, knot A.! (i `rem` 256)) | i <- [from..from+len-1], let ic = (from + from + len - 1 - i) `rem` 256]
    straights = [(ic, knot A.! ic) | i <- [from+len..from+256-1], let ic = i `rem` 256]

hash :: [Int] -> State HashState ()
hash is = do
  forM_ is $ \i -> do
    (HashState cp ss h) <- get
    nextHash <- pure $ revert cp i h
    modify' $ \s -> s { _currentPos = cp + ss + i, _skipSize = ss + 1, _hash = nextHash }

pt1 :: State HashState Int
pt1 = do
  es <- gets _hash
  return (es A.! 0 * es A.! 1)

toLengths :: String -> [Int]
toLengths = map readInt . splitOn ","

partOneI :: String -> Int
partOneI is = evalState (hash lens >> pt1) (HashState 0 0 $ mkKnot (zip [0..] [0..255]))
  where lens = toLengths is

tweak :: String -> [Int]
tweak is = map ord is ++ [17, 31, 73, 47, 23]

dense :: [Int] -> [Int]
dense is = map mix (chunksOf 16 is)
  where mix = \xs -> foldr xor (head xs) (tail xs)

toHex :: [Int] -> String
toHex = concatMap go
  where
    go 0 = "00"
    go i = showHex i ""

runHash :: [Int] -> State HashState ()
runHash is = do
  forM_ [1..64] $ \_ -> hash is

partTwoI :: String -> String
partTwoI is = toHex $ dense . A.elems . _hash $ execState (runHash lens) (HashState 0 0 $ mkKnot (zip [0..] [0..255]))
  where
    lens = tweak is

instance Challenge String where
  parse = id
  partOne = show . partOneI
  partTwo = partTwoI