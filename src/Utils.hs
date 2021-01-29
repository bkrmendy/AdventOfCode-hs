module Utils where

import Text.Parsec as Parsec
import qualified Data.Map as Map
import Control.Monad (zipWithM)
import Data.Char (digitToInt)
import Data.List (foldl', find)
import Data.Word8
import qualified Data.ByteString as Bs
import qualified Data.ByteString.Base16 as B16
import qualified Crypto.Hash.MD5 as MD5
import qualified Data.Set as Set

-- | LISTS
transpose :: [[a]] -> [[a]]
transpose ([]:_) = []
transpose x = map head x : transpose (map tail x)

-- ^ https://stackoverflow.com/a/21288092
subsequencesOfSize :: Int -> [a] -> [[a]]
subsequencesOfSize n xs = let l = length xs
                          in if n>l then [] else subsequencesBySize xs !! (l-n)
 where
   subsequencesBySize [] = [[[]]]
   subsequencesBySize (x:xs) = let next = subsequencesBySize xs
                             in zipWith (++) ([]:next) (map (map (x:)) next ++ [[]])

-- ^ https://stackoverflow.com/a/60380502
replace :: (Num a, Enum a, Eq a) => a -> c -> [c] -> [c]
replace index element = zipWith (curry transform) [0 ..]
  where
    transform (i, e)
      | i == index = element
      | otherwise = e

concatRep :: Int -> String -> String
concatRep n = concat . replicate n

remove :: Int -> [a] -> [a]
remove _ [] = []
remove 0 (_:rest) = rest
remove n (hd:rest) = hd:remove (pred n) rest

insert :: a -> Int -> [a] -> [a]
insert a 0 elems = a:elems
insert _ _ [] = []
insert a n (hd:rest) = hd : insert a (pred n) rest

-- | SET
listify :: (Ord a) => (a -> Set.Set a -> Set.Set a) -> [a] -> Set.Set a -> Set.Set a
listify _ [] set = set
listify f (a:as) set = listify f as (f a set)

inserts :: (Ord a) => [a] -> Set.Set a -> Set.Set a
inserts = listify Set.insert

deletes :: (Ord a) => [a] -> Set.Set a -> Set.Set a
deletes = listify Set.delete

-- | PARSING

-- ^ https://www.schoolofhaskell.com/user/stevely/parsing-floats-with-parsec#parsing-integers-with-leading-sign
int :: Parsec.Parsec String () Int
int = rd <$> (plus <|> minus <|> number)
  where rd     = read :: String -> Int
        plus   = char '+' *> number
        minus  = (:) <$> char '-' <*> number
        number = many1 digit
        
integer :: Parsec.Parsec String () Integer
integer = toInteger <$> int        

parseLines :: Parsec.Parsec String () a -> String -> a
parseLines parser = parseI
  where
    parseI input =
      case Parsec.parse parser "" input of
        Left err -> error $ show err
        Right commands -> commands

-- ^ adapted from https://stackoverflow.com/a/26961027
fromBinaryString :: String -> Int
fromBinaryString = foldl' (\acc x -> acc * 2 + digitToInt x) 0

-- | UNWRAP

unsafeGet :: (Ord k) => k -> Map.Map k a -> a
unsafeGet = Map.findWithDefault undefined

unsafeFromMaybe :: Maybe a -> a
unsafeFromMaybe m =
  case m of
    Nothing -> error "Maybe contains no value!"
    Just something -> something
    
-- | HASH
hash :: Bs.ByteString -> Bs.ByteString
hash = Bs.pack . map toLower . Bs.unpack . B16.encode . MD5.hash

-- | Combinator
s :: (t1 -> t2 -> t3) -> (t1 -> t2) -> t1 -> t3
s f g x = f x (g x)

 
-- | CHINESE REMAINDER THEOREM
-- ^ adapted from https://rosettacode.org/wiki/Chinese_remainder_theorem#Haskell
chineseRemainder :: [Integer] -> [Integer] -> Maybe Integer
chineseRemainder residues modulii =
  zipWithM modInv crtModulii modulii >>=
  (Just . (`mod` modPI) . sum . zipWith (*) crtModulii . zipWith (*) residues)
  where
    modPI = product modulii
    crtModulii = (modPI `div`) <$> modulii

    egcd :: Integer -> Integer -> (Integer, Integer)
    egcd _ 0 = (1, 0)
    egcd a b = (t, s - q * t)
      where
        (s, t) = egcd b r
        (q, r) = a `quotRem` b

    modInv :: Integer -> Integer -> Maybe Integer
    modInv a b =
      case egcd a b of
        (x, y)
          | a * x + b * y == 1 -> Just x
          | otherwise -> Nothing

factorial :: (Integral a) => a -> a
factorial 0 = 1
factorial n = n * factorial (pred n)