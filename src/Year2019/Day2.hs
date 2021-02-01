{-# LANGUAGE FlexibleInstances #-}
module Year2019.Day2 where
import Challenge
import Utils
import Control.Monad.State
import qualified Intcode as IC

doctor :: Int -> Int -> IC.Program -> IC.Program
doctor noun verb (IC.Program program) = IC.Program $ replace 1 noun $ replace 2 verb program

combo :: IC.Program -> [Int]
combo mem = [100 * n + v | n <- [0..100], v <- [0..100]
                         , IC.runCode (doctor n v mem) == 19690720]

instance Challenge IC.Program where
  parse = doctor 12 2 . IC.fromString
  partOne = show . IC.runCode
  partTwo = show . combo