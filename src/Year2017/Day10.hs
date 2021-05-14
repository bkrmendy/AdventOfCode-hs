module Year2017.Day10 where
import            Challenge
import            Utils (readInt)
import            KnotHash

import            Data.Char (ord)
import            Data.List.Split (splitOn)
import            Control.Monad.State
import qualified  Data.Array as A

pt1 :: State HashState Int
pt1 = do
  es <- gets _hash
  return (es A.! 0 * es A.! 1)

toLengths :: String -> [Int]
toLengths = map readInt . splitOn ","

partOneI :: String -> Int
partOneI is = evalState (hash lens >> pt1) (HashState 0 0 $ mkKnot (zip [0..] [0..255]))
  where lens = toLengths is

instance Challenge String where
  parse = id
  partOne = show . partOneI -- 38628
  partTwo = knotHash        -- e1462100a34221a7f0906da15c1c979a