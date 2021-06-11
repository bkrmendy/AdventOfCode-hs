{-# LANGUAGE CPP #-}
module Main where

import Challenge
import Intcode
import Year2018.Day8
import System.FilePath (takeDirectory, (</>))

baseDir :: String
baseDir = takeDirectory $ takeDirectory __FILE__

runChallenge :: IO ()
runChallenge = do
  input <- readFile $ baseDir </> "input/2018/8.txt"
  let parsed = parse input :: Node
  putStrLn $ partOne parsed
  putStrLn $ partTwo parsed

main :: IO ()
main = runChallenge
