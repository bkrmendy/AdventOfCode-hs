module Year2019.Day5 where
import Challenge
import qualified Intcode as IC

instance Challenge IC.Program where
  parse = IC.fromString
  partOne = show . last . IC.executeCode [1]
  partTwo = show . last . IC.executeCode [5]
