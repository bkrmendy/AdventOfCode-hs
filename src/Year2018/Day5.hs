module Year2018.Day5 where

import Challenge

import Data.Char (toLower, isUpper, isLower)

react :: String -> String
react (a:b:rest)
  | isUpper a && isLower b && toLower a == b = react rest
  | isUpper b && isLower a && toLower b == a = react rest
  | otherwise = a:react (b:rest)
react xs = xs

runReactions :: String -> String
runReactions molecule
  | nextMolecule == molecule = molecule
  | otherwise = runReactions nextMolecule
  where
    nextMolecule = react molecule

remove :: String -> [String]
remove ms = [ filter (\l -> toLower l /= toLower letter) ms | letter <- ['a'..'z'] ]

instance Challenge String where
  parse = id
  partOne = show . length . runReactions
  partTwo = show . minimum . map (length . runReactions) . remove

