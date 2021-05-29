{-# LANGUAGE CPP #-}
module Main where

import Challenge
<<<<<<< HEAD
import Intcode
import Year2019.Day17
import System.FilePath (takeDirectory, (</>))

baseDir :: String
baseDir = takeDirectory $ takeDirectory __FILE__

runChallenge :: IO ()
runChallenge = do
  input <- readFile $ baseDir </> "input/2019/17.txt"
  let parsed = parse input :: Program
  putStrLn $ partOne parsed
  putStrLn $ partTwo parsed

main :: IO ()
main = runChallenge
