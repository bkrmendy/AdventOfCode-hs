module Year2019.Day7 where
import Challenge
import Data.List (permutations, sortOn)
import qualified Intcode as IC
import Control.Monad.State

newtype Setting = Setting { unSetting :: Int }

fixed :: Eq a => (a -> a) -> a -> a
fixed f a 
  | a == a' = a
  | otherwise = fixed f a'
  where a' = f a

type Strategy = [IC.ProcessState] -> Int -> Int

runAmps :: [IC.ProcessState] -> Int -> ([IC.ProcessState], Int)
runAmps [] input = ([], input)
runAmps (amp:amps) input = (amp2:rest, final)
    where
      (out, amp2) = runState (do
                      IC.addProcessInputs [input]
                      IC.continueExecution) amp
      (rest, final) = runAmps amps (last out)
        
runAmpsSerial :: Strategy
runAmpsSerial amplifiers seed = snd (runAmps amplifiers seed) 

runAmpsLinked :: Strategy
runAmpsLinked amplifiers out
  | IC._status (last amplifiers) == IC.Terminated = out
  | otherwise = runAmpsLinked amplifiers2 out2
  where
    (amplifiers2, out2) = runAmps amplifiers out 

maxThrust :: [([Setting], Int)] -> Int
maxThrust = snd . head . sortOn (negate . snd)

run :: [Setting] -> Strategy -> IC.Program -> Int
run settings strategy program = maxThrust $ map (\s -> (s, strategy (fromSetting s) 0)) (permutations settings)
  where
    fromSetting setting = [IC.initializeProcess program [i] | (Setting i) <- setting]

instance Challenge IC.Program where
  parse = IC.fromString
  partOne = show . run (map Setting [0..4]) runAmpsSerial
  partTwo = show . run (map Setting [5..9]) runAmpsLinked