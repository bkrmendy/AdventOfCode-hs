module Main where
  
import Challenge
import Year2021.Day14
import AdventUtils (readInput)

runChallenge :: IO ()
runChallenge = do
  input <- readInput "2021" "14"
  let parsed = parse input :: Polymerization
  putStrLn $ partOne parsed
  putStrLn $ partTwo parsed

main :: IO ()
main = runChallenge
