module Utils where

import Text.Parsec as Parsec
import qualified Data.Map as Map

transpose :: [[a]] -> [[a]]
transpose ([]:_) = []
transpose x = map head x : transpose (map tail x)

-- ^ https://stackoverflow.com/a/60380502
replace :: (Num a, Enum a, Eq a) => a -> c -> [c] -> [c]
replace index element = zipWith (curry transform) [0 ..]
  where
    transform (i, e)
      | i == index = element
      | otherwise = e

concatRep :: Int -> String -> String
concatRep n = concat . replicate n

-- ^ https://www.schoolofhaskell.com/user/stevely/parsing-floats-with-parsec#parsing-integers-with-leading-sign
int :: Parsec.Parsec String () Int
int = rd <$> (plus <|> minus <|> number)
  where rd     = read :: String -> Int
        plus   = char '+' *> number
        minus  = (:) <$> char '-' <*> number
        number = many1 digit

parseLines :: Parsec.Parsec String () a -> String -> a
parseLines parser = parseI
  where
    parseI input =
      case Parsec.parse parser "" input of
        Left err -> error $ show err
        Right commands -> commands


unsafeGet :: (Ord k) => k -> Map.Map k a -> a
unsafeGet = Map.findWithDefault undefined