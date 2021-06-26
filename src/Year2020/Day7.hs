module Year2020.Day7 where

import Challenge
import Utils (readInt)

import qualified Data.Map.Strict as M
import Control.Monad.State.Strict

data Bag = Bag { _attr :: String, _color :: String } deriving (Eq, Ord, Show)

type Rules = M.Map Bag [(Int, Bag)]

bags :: [String] -> [(Int, Bag)]
bags (n:attr:color:_:rest) = (readInt n, Bag attr color):bags rest
bags _                     = []

rule :: String -> (Bag, [(Int, Bag)])
rule i = case words i of
  attr:color:"bags":"contain":"no":_ -> (Bag attr color, [])
  attr:color:"bags":"contain":bs -> (Bag attr color, bags bs)

rules :: [String] -> State Rules ()
rules []   = return ()
rules (i:is) = do
  let (b, bs) = rule i
  modify' $ M.insertWith (++) b bs
  rules is

parseI :: [String] -> Rules
parseI = flip execState M.empty . rules

canHoldShiny :: Bag -> Rules -> Bool
canHoldShiny bag rls = case M.lookup bag rls of
  Nothing -> False
  Just bs -> any (\(_, b) -> b == shiny || canHoldShiny b rls) bs
  where shiny = Bag "shiny" "gold"

containsNBags :: Bag -> Rules -> Int
containsNBags bag rls = case M.lookup bag rls of
  Nothing -> 0
  Just [] -> 1
  Just bs -> 1 + sum (map (\(i, b) -> i * containsNBags b rls) bs)

instance Challenge Rules where
  parse = parseI . lines
  partOne rls = show . length $ filter (`canHoldShiny` rls) (M.keys rls)
  partTwo = show . pred . containsNBags (Bag "shiny" "gold")
