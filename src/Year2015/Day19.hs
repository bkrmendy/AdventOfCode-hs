{-# LANGUAGE FlexibleInstances #-}
module Year2015.Day19 where
import Challenge
import Data.List (sortOn)
import Data.List.Split (splitOn)
import qualified Data.Sequence as Seq
import qualified Data.Set as Set

data Replacement = Replacement { lhs :: String, rhs :: String}

parseI line = Replacement a b
  where
    (a:b:_) = splitOn " => " line

deplace :: [Replacement] -> [Replacement]
deplace = map (\(Replacement from to) -> Replacement to from)

replace :: String -> String -> String -> String
replace str from to = to ++ drop (length from) str

replacementsOf :: String -> Replacement -> [String]
replacementsOf [] _ = []
replacementsOf str (Replacement from "e") = ["e" | from == str]
replacementsOf str (Replacement from to) = replI str
    where
      replI :: String -> [String]
      replI s
        | null s = []
        | take (length from) s == from = replace s from to:[head s:re | re <- replI (tail s)]
        | otherwise = [head s:re | re <- replI (tail s)]

replacements :: String -> [Replacement] -> Set.Set String
replacements str = foldr go Set.empty
  where
    go re se = Set.union se $ Set.fromList $ replacementsOf str re

shrinkBfs :: String -> [Replacement] -> Int
shrinkBfs str reps = shrinkI (Set.singleton str) (Seq.singleton (0, str))
  where
    shrinkI :: Set.Set String -> Seq.Seq (Int, String) -> Int
    shrinkI seen queue
      | thisString == "e" = steps
      | otherwise = shrinkI nextSeen nextQueue
      where
        (steps, thisString) = Seq.index queue 0
        theseReplacements = Set.filter (not . flip Set.member seen) $ replacements thisString reps
        nextSeen = Set.union seen theseReplacements
        nextQueue = Seq.drop 1 queue Seq.>< Seq.fromList (map ((,) (succ steps)) (Set.elems nextSeen))
        
tryReplace :: Replacement -> (Int, String) ->  (Int, String)
tryReplace rep (n, str) =
  case replacementsOf str rep of
    []    -> (n, str)
    (a:_) -> (n + 1, a)
 
shrinkGreedy :: Int -> String -> [Replacement] -> Int
shrinkGreedy steps "e" _    = steps
shrinkGreedy steps str reps = shrinkGreedy (steps + n) asd reps
  where
    (n, asd) = foldr tryReplace (0, str) reps

molecule :: String
molecule = "CRnSiRnCaPTiMgYCaPTiRnFArSiThFArCaSiThSiThPBCaCaSiRnSiRnTiTiMgArPBCaPMgYPTiRnFArFArCaSiRnBPMgArPRnCaPTiRnFArCaSiThCaCaFArPBCaCaPTiTiRnFArCaSiRnSiAlYSiThRnFArArCaSiRnBFArCaCaSiRnSiThCaCaCaFYCaPTiBCaSiThCaSiThPMgArSiRnCaPBFYCaCaFArCaCaCaCaSiThCaSiRnPRnFArPBSiThPRnFArSiRnMgArCaFYFArCaSiRnSiAlArTiTiTiTiTiTiTiRnPMgArPTiTiTiBSiRnSiAlArTiTiRnPMgArCaFYBPBPTiRnSiRnMgArSiThCaFArCaSiThFArPRnFArCaSiRnTiBSiThSiRnSiAlYCaFArPRnFArSiThCaFArCaCaSiThCaCaCaSiRnPRnCaFArFYPMgArCaPBCaPBSiRnFYPBCaFArCaSiAl"

moleculeEx :: String
moleculeEx = "HOHOHO"

instance Challenge [Replacement] where
  parse = map parseI . lines
  partOne = show . Set.size . replacements molecule
  partTwo = show . shrinkGreedy 0 molecule . deplace . sortOn rhs
