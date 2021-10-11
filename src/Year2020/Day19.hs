module Year2020.Day19 where

import Prelude hiding (sequence)

import Utils (int, parseLines, space, countWhere)
import Challenge
  
import Text.Parsec (Parsec, try, char, sepBy1, many1, letter, string, manyTill, newline, between)
import qualified Data.Map.Strict as M
import Control.Applicative
import Data.Bifunctor (first)

data Rule = Character Char
          | Sequence [Int]
          | Alternative [Int] [Int]
     
type Rules = M.Map Int Rule

type Puzzle = (Rules, [String])

character, sequence, alternative, matcher :: Parsec String () Rule
matcher     = character <|> try sequence <|> alternative 
character   = Character   <$> between (char '"') (char '"') letter
sequence    = Sequence    <$> sepBy1 int space
alternative = Alternative <$> manyTill (int <* space) (string "| ") <*> sepBy1 int space

rule :: Parsec String () (Int, Rule)
rule = (,) <$> (int <* string ": ") <*> matcher

puzzle :: Parsec String () (Rules, [String])
puzzle = (,) <$> (rules <* newline) <*> strings
  where rules = M.fromList <$> many (rule <* newline)
        strings = sepBy1 (many1 letter) newline

match :: Rules -> Rule -> String -> [String]
match _ (Character _) "" = []
match _ (Character c) (h:rest)
  | c == h = [rest]
  | otherwise = []

match _ (Sequence []) source = [source]
match _ (Sequence _) ""      = []
match rules (Sequence (r:rs)) source = do
  let ruleResolved = rules M.! r 
  rest <- match rules ruleResolved source
  match rules (Sequence rs) rest

match rules (Alternative as bs) source = left <|> right
  where left  = match rules (Sequence as) source
        right = match rules (Sequence bs) source

tweak :: Rules -> Rules
tweak rules =  mods <> rules
  where mods = M.fromList [ (8, Alternative  [42] [42, 8])
                          , (11, Alternative [42, 31] [42, 11, 31])
                          ]  

good :: [String] -> Bool
good = elem ""

solve :: Rules -> [String] -> Int
solve rules = countWhere go
  where go = good . match rules (rules M.! 0)
        
instance Challenge Puzzle where
  parse = parseLines puzzle
  partOne = show . uncurry solve
  partTwo = show . uncurry solve . first tweak

