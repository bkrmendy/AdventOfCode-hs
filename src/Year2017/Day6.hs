module Year2017.Day6 where
import Challenge
import Utils (readInt)
import Control.Monad.State.Lazy
import qualified Data.Set as S
import qualified Data.Array as A
import Data.List (elemIndex)

type Bank = A.Array Int Int
data Bookkeeping = Bookkeeping { _steps :: Int, _seen :: S.Set Bank }

parseI :: String -> A.Array Int Int
parseI str = A.array (0, length is - 1) (zip [0..] is)
  where is = readInt <$> words str

redistribute :: Bank -> Bank
redistribute bank = A.accum (+) bank ((maxIx, -maxElement) : [ (j `rem` n, 1) | j <- [maxIx+1..maxIx+maxElement]])
  where
    es = A.elems bank
    maxElement = maximum es
    Just maxIx = elemIndex maxElement es
    n = length es

reallocate :: Bank -> State Bookkeeping [Int]
reallocate banks = do
  (Bookkeeping steps seen) <- get
  nextBanks <- pure $ redistribute banks
  if S.member banks seen
    then do
      put (Bookkeeping 0 S.empty)
      (:) steps <$> reallocate nextBanks
    else do
      put (Bookkeeping (steps + 1) (S.insert banks seen))
      reallocate nextBanks

solve :: Bank -> (Int, Int)
solve bank = (a, c)
  where
    (a:c:_) = evalState (reallocate bank) (Bookkeeping 0 S.empty)

instance Challenge Bank where
  parse = parseI
  partOne = show . fst . solve
  partTwo = show . snd . solve