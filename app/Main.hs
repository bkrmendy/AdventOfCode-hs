{-# LANGUAGE CPP #-}
module Main where

import Challenge
import Intcode
import Year2019.Day23
import System.FilePath (takeDirectory, (</>))

baseDir :: String
baseDir = takeDirectory $ takeDirectory __FILE__

runChallenge :: IO ()
runChallenge = do
  input <- readFile $ baseDir </> "input/2019/23.txt"
  let parsed = parse input :: Program
  putStrLn $ partOne parsed
  putStrLn $ partTwo parsed

main :: IO ()
main = runChallenge
