{-# LANGUAGE FlexibleInstances #-}
module Year2015.Day2 where
import Challenge
import Utils (int, parseLines)
import Text.Parsec
import Data.List (sort)

data Measurement = Measurement { _length :: Int, _width :: Int, _height :: Int }

parseI :: Parsec String () Measurement
parseI = Measurement <$> int <*> (char 'x' *> int) <*> (char 'x' *> int)

wrapper :: Measurement -> Int
wrapper (Measurement l w h) = 2*l*w + 2*w*h + 2*h*l + minimum [l*w, w*h, h*l]

ribbon :: Measurement -> Int
ribbon (Measurement l w h) = 2*a + 2*b
  where [a, b, _] = sort [l, w, h]

bow :: Measurement -> Int
bow (Measurement l w h) = l * w * h

package :: [Measurement -> Int] -> Measurement -> Int
package fs m = sum (map ($ m) fs)

solve :: (Measurement -> Int) -> [Measurement] -> Int
solve f = foldr (\m a -> a + f m) 0

instance Challenge [Measurement] where
  parse = parseLines (sepBy1 parseI newline)
  partOne = show . solve (package [wrapper])
  partTwo = show . solve (package [bow, ribbon])

