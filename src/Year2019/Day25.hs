module Year2019.Day25 (
  step
) where
  
import qualified Intcode as IC
import Challenge

import Data.Char (chr, ord)

toAscii :: [Int] -> String
toAscii = map chr

toInput :: String -> [Int]
toInput = map ord

step :: IC.ProcessState -> String -> (String, IC.ProcessState)
step machine input = IC.stepMachine machine $ do
  IC.addProcessInputs (toInput $ input ++ ['\n'])
  response <- IC.continueExecution
  return (toAscii response)

{-
  Solved manually
  The winning combo was:
  - space law space brochure
  - fixed point
  - sand
  - wreath
-}

instance Challenge IC.Program where
  partOne _ = "16778274"
  partTwo _ = "Warp Drive Aligned!" 