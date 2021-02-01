module Year2019.Day5 where
import Challenge
import qualified Intcode as IC

instance Challenge IC.Program where
  parse = IC.fromString
  partOne = show . IC.executeCode [5]
  partTwo _ = "unknown"
