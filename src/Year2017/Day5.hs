module Year2017.Day5 where
import Challenge
import Utils (readInt)
import qualified Data.Array.ST as A
import Control.Monad.ST
import Control.Monad.Loops (whileM_)
import Data.STRef

type Increment = Int -> Int
byOne :: Increment
byOne n = n + 1

threeOrMore :: Increment
threeOrMore n
  | n >= 3 = n -1
  | otherwise = n + 1

run :: Increment -> [Int] -> Int
run f is = runST $ do
  instructions <- A.newListArray (0, length is - 1) is :: ST s (A.STArray s Int Int)
  iPtr <- newSTRef 0
  steps <- newSTRef 0
  running <- newSTRef True
  whileM_ (readSTRef running) $ do
    iPtrValue <- readSTRef iPtr
    jump <- A.readArray instructions iPtrValue
    A.writeArray instructions iPtrValue (f jump)
    modifySTRef' iPtr (+ jump)
    modifySTRef' steps (+ 1)
    bounds <- A.getBounds instructions
    writeSTRef running (A.inRange bounds (iPtrValue + jump))
  readSTRef steps

instance Challenge [Int] where
  parse = map readInt . lines
  partOne = show . run byOne
  partTwo = show . run threeOrMore
