{-# LANGUAGE CPP #-}
module Main where

import Challenge
import Year2015.Day6
import System.FilePath (takeDirectory, (</>))

baseDir :: String
baseDir = takeDirectory $ takeDirectory __FILE__

runChallenge :: IO ()
runChallenge = do
  input <- readFile $ baseDir </> "input/2015/6.txt"
  let parsed = parse input :: [Instruction]
  putStrLn $ partOne parsed
  putStrLn $ partTwo parsed

main :: IO ()
main = runChallenge
