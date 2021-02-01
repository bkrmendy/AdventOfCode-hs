module Year2019.Day9 where
import Challenge
import qualified Intcode as IC

instance Challenge IC.Program where
  parse = IC.fromString
  partOne = show . head . IC.executeCode [1]
  partTwo = show . head . IC.executeCode [2]
