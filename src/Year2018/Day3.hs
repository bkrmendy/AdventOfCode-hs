module Year2018.Day3 where
import Challenge
import Utils (parseL, int, deletes)

import Text.Parsec
import qualified Data.Map.Strict as M
import qualified Data.Set as S

data Claim = Claim { _id :: Int, _left :: Int, _top :: Int, _width :: Int, _height :: Int }

-- | #123 @ 3,2: 5x4
claim :: Parsec String () Claim
claim = Claim <$> (char '#' *> int) <*> (string " @ " *> int) <*> (char ',' *> int) <*> (string ": " *> int) <*> (char 'x' *> int)

type Fabric = M.Map (Int, Int) [Int]

squareInches :: Claim -> [((Int, Int), Int)]
squareInches (Claim cid left top width height) = [((l, t), cid) | l <- [left..left + width - 1], t <- [top..top+height - 1]]

put :: Claim -> Fabric -> Fabric
put c f = foldr (\(s, i) -> M.insertWith (++) s [i]) f (squareInches c)

fabric :: [Claim] -> Fabric
fabric = foldr put M.empty

nNonUniqueSquares :: Fabric -> Int
nNonUniqueSquares = M.size . M.filter ((>= 2) . length)

uniqueClaimId :: [Claim] -> Int
uniqueClaimId claims = head . S.elems $ foldr take claimIds (M.assocs f)
  where
    f = fabric claims
    claimIds = S.fromList (map _id claims)
    take :: ((Int, Int), [Int]) -> S.Set Int -> S.Set Int
    take (_, [_]) c = c
    take (_, is) c = deletes is c

instance Challenge [Claim] where
  parse = parseL claim
  partOne = show . nNonUniqueSquares . fabric
  partTwo = show . uniqueClaimId

