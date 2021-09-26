module Year2020.Day16 where

import Utils (int, transpose)
import Challenge

import Data.Either (either)
import Data.List (isPrefixOf)

import Text.Parsec as P
import qualified Data.Map.Strict as M

-- | DATA  
  
data Range = Range { _lower :: Int, _upper :: Int } deriving (Show)
  
type Ticket = [Int]
type Places = [Int]
type Fields = M.Map String (Range, Range)

data Notes = Notes { _fields :: Fields
                   , _yourTicket :: Ticket
                   , _nearbyTickets :: [Ticket]
                   }

-- | PARSING

range :: Parsec String () Range
range = Range <$> int <*> (string "-" *> int)

rangeOr :: Parsec String () (Range, Range)
rangeOr = (,) <$> range <*> (string " or " *> range)

field :: Parsec String () (String, (Range, Range))
field = (,) <$> name <*> (string ": " *> rangeOr)
  where name = many1 (letter <|> space)
        
notes :: Parsec String () Notes
notes = do
   fields <- M.fromList <$> manyTill (field <* newline) newline
   _ <- string "your ticket:" *> newline
   yourTicket <- tickets <* newline
   _ <- newline
   _ <- string "nearby tickets:" >> newline
   nearbyTickets <- many1 (tickets <* newline)
   return $ Notes fields yourTicket nearbyTickets
   where tickets = sepBy1 int (char ',')
   
-- | UTIL   
   
inRange :: Range -> Int -> Bool
inRange (Range l u) n = l <= n && n <= u

-- | PART 1 

valids :: [(Range, Range)] -> [Ticket] -> ([Int], [Ticket])
valids ranges ts = go ts ([], []) 
  where 
        inRanges n = any (\(ra, rb) -> inRange ra n || inRange rb n) ranges
    
        validI :: Ticket -> Int
        validI = sum . filter (not . inRanges)
        
        go :: [Ticket] -> ([Int], [Ticket]) -> ([Int], [Ticket])
        go [] acc             = acc
        go (t:rest)  (invalids, vs)  = case validI t of
          0 -> go rest (invalids, t:vs)
          n -> go rest (n:invalids, vs)
 
partOneI :: Notes -> Int
partOneI (Notes fields _ nearbyTickets) = sum . fst $ valids ranges nearbyTickets
  where ranges = M.elems fields

-- | PART 2

type Cipher = M.Map String Int

decipher :: Cipher -> Ticket -> Int
decipher cipher ticket = product [ ticket !! v | (f, v) <- M.assocs cipher, "departure" `isPrefixOf` f]

funnel :: Fields -> Places -> Fields
funnel fs p = M.filter validI fs
  where validI (ra, rb) = all (\i -> inRange ra i || inRange rb i) p 

searchSingletonFields :: Fields -> [(Int, Places)] -> (String, Int)
searchSingletonFields _      []               = error "Out of places!"
searchSingletonFields fields ((i, p):places)
  | M.size funneled == 1 = (head $ M.keys funneled, i)
  | otherwise = searchSingletonFields funneled places 
  where funneled = funnel fields p

mkCipher :: [Ticket] -> Fields -> Cipher
mkCipher ts fields = M.fromList $ go fields (M.fromList $ zip [0..] places)
  where places = transpose ts
  
        go :: Fields -> M.Map Int Places -> [(String, Int)]
        go fs _ | M.null fs = []
        go fs ps = (key, i):go nextFs nextPs
          where (key, i) = searchSingletonFields fs (M.assocs ps)
                nextFs = M.delete key fs
                nextPs = M.delete i ps

partTwoI :: Notes -> Int
partTwoI (Notes fs yt nt) = decipher cipher yt
  where ranges = M.elems fs
        validTickets = snd $ valids ranges nt
        cipher = mkCipher validTickets fs
   
instance Challenge Notes where
  parse input = either (error . show) id (P.parse notes "" input)
  partOne = show . partOneI
  partTwo = show . partTwoI  