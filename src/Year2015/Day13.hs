module Year2015.Day13 where
import Challenge
import Utils
import Text.Parsec
import Data.List (permutations, nub)

data Preference = Preference { person :: String, hu :: Int, other :: String }

parseI = Preference <$> (many1 letter <* string " would ")
                    <*> (((negate <$ string "lose") <|> (id <$ string "gain")) <*> (string " " *> int))
                    <*> (string " happiness units by sitting next to " *> many1 letter <* char '.')

tsp :: [Preference] -> [Int]
tsp edges = map walk (permutations names)
  where
    names = nub $ map person edges
    edge f t = hu $ head $ [ e | e <- edges
                               , person e == f
                               , other e == t]
    walkI [] = [0]
    walkI [a, b] = [edge a b]
    walkI (a:b:rest) = edge a b:walkI (b:rest)

    walk ps =
      let
        around psi = sum (edge (last psi) (head psi):walkI psi)
      in around ps + around (reverse ps)

me :: [Preference] -> [Preference]
me prefs = prefs ++ myself
  where
    names = nub $ map person prefs
    myself = concat [[Preference "Berci" 0 name, Preference name 0 "Berci"] | name <- names]

instance Challenge [Preference] where
  parse = parseLines (sepBy1 parseI newline)
  partOne = show . maximum . tsp
  partTwo = show . maximum . tsp . me
