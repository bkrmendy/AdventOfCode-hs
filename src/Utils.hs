module Utils where

import Text.Parsec as Parsec

transpose :: [[a]] -> [[a]]
transpose ([]:_) = []
transpose x = map head x : transpose (map tail x)

-- ^ https://stackoverflow.com/a/60380502
replace index elem = zipWith (curry transform) [0 ..]
  where
    transform (i, e)
      | i == index = elem
      | otherwise = e

concatRep :: Int -> String -> String
concatRep n = concat . replicate n

int :: Parsec.Parsec String () Int
int = read <$> Parsec.many1 Parsec.digit

parseLines :: Parsec.Parsec String () a -> String -> a
parseLines parser = parse
  where
    parse input =
      case Parsec.parse parser "" input of
        Left err -> error $ show err
        Right commands -> commands