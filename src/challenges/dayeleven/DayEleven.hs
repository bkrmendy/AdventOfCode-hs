{-# LANGUAGE FlexibleInstances #-}
module DayEleven where
import Prelude hiding (floor)
import Data.List (nubBy)
import qualified Data.Sequence as Seq
import qualified Data.Set as Set

import Challenge

data Element
  = Polonium
  | Thulium
  | Promethium
  | Lithium
  | Hydrogen
  | Ruthenium
  | Elerium
  | Dilithium
  | Cobalt deriving (Eq, Ord, Show)

data Component = Microchip | Generator deriving (Eq, Ord, Show)

type Part = (Component, Element)
type Floor = Set.Set Part

listify :: (Ord a) => (a -> Set.Set a -> Set.Set a) -> [a] -> Set.Set a -> Set.Set a
listify _ []     s = s
listify f (e:es) s = listify f es (f e s)

insert :: (Ord a) => [a] -> Set.Set a -> Set.Set a
insert = listify Set.insert

delete :: (Ord a) => [a] -> Set.Set a -> Set.Set a
delete = listify Set.delete

-- if a chip is ever left in the same area as another RTG, and it's not connected to its own RTG, the chip will be fried
valid :: [Floor] -> Bool
valid = all chipsHaveGens
  where
    chipsHaveGens fl = and [Set.member (Generator, m) fl | (Microchip, m) <- Set.elems fl
                                                         , (Generator, g)  <- Set.elems fl
                                                         , m /= g]

finished :: [Floor] -> Bool
finished [f, s, t, _] = Set.null f && Set.null s && Set.null t
finished _            = False

-- elevator: min 1x (Microchip | Generator), max 2x (Microchip | Generator)

moveUp :: Floor -> Floor -> [[Floor]]
moveUp from to = filter valid (single ++ double)
  where
    single = [[Set.delete e from, Set.insert e to] | e <- Set.elems from]
    double = [[delete [e1, e2] from, insert [e1, e2] to] | e1 <- Set.elems from
                                                         , e2 <- Set.elems from
                                                         , e1 /= e2]

moveDown :: Floor -> Floor -> [[Floor]]
moveDown to from = filter valid (single ++ double)
  where
    single = [[Set.insert e to, Set.delete e from] | e <- Set.elems from]
    double = [[insert [e1, e2] to, delete [e1, e2] from] | e1 <- Set.elems from
                                                         , e2 <- Set.elems from
                                                         , e1 /= e2]

moves :: Int -> [Floor] -> [(Int, [Floor])]
-- move up to b
moves 0 [a, b, c, d] = map (\ms -> (1, ms ++ [c, d])) (moveUp a b)
-- move up to c, move down to a
moves 1 [a, b, c, d] = up ++ down
  where
    up = map (\ms -> (2, [a] ++ ms ++ [d])) (moveUp b c)
    down = if Set.null a then [] else map (\ms -> (0, ms ++ [c, d])) (moveDown a b)
-- move up to d, move down to c
moves 2 [a, b, c, d] = up ++ down
  where
    up = map (\ms -> (3, [a, b] ++ ms)) (moveUp c d)
    down = if Set.null a && Set.null b then [] else map (\ms -> (1, [a] ++ ms ++ [d])) (moveDown b c)
-- move up to c, move down to a
moves 3 [a, b, c, d] = map (\ms -> (2, [a, b] ++ ms)) (moveDown c d)
moves _ _ = error "I have no memory of this place"

equivalent :: (Int, [Floor]) -> (Int, [Floor]) -> Bool
equivalent (_, f1) (_, f2) = and (zipWith equivI f1 f2)
  where
    equivI :: Floor -> Floor -> Bool
    equivI fl1 fl2 = Set.map snd fl1 == Set.map snd fl2
 

solve :: Set.Set (Int, [Floor]) -> Seq.Seq (Int, (Int, [Floor])) -> Int
solve cache queue
  | finished thisFloors = steps
  | otherwise = solve nextCache (nextQueue Seq.>< Seq.fromList (map ((,) (succ steps)) nextMoves))
  where
    (steps, (level, thisFloors)) = Seq.index queue 0
    prune ms =  nubBy equivalent $ [m | m <- ms, not (Set.member m cache)]
    nextQueue = Seq.drop 1 queue
    nextMoves = prune (moves level thisFloors)
    nextCache = insert nextMoves cache

floors1 :: [Floor]
floors1 = [ Set.fromList [(Generator, Polonium), (Generator, Thulium), (Microchip, Thulium), (Generator, Promethium), (Generator, Ruthenium), (Microchip, Ruthenium), (Generator, Cobalt), (Microchip, Cobalt) ]
          , Set.fromList [(Microchip, Polonium), (Microchip, Promethium)]
          , Set.empty
          , Set.empty
          ]
         
floors2 :: [Floor]
floors2 = [ Set.fromList [(Generator, Elerium), (Microchip, Elerium), (Generator, Dilithium), (Microchip, Dilithium), (Generator, Polonium), (Generator, Thulium), (Microchip, Thulium), (Generator, Promethium), (Generator, Ruthenium), (Microchip, Ruthenium), (Generator, Cobalt), (Microchip, Cobalt) ]
          , Set.fromList [(Microchip, Polonium), (Microchip, Promethium)]
          , Set.empty
          , Set.empty
          ]

floorsEx :: [Floor]
floorsEx = [ Set.fromList [(Microchip,Lithium), (Microchip,Hydrogen)]
           , Set.fromList [(Generator,Hydrogen)]
           , Set.fromList [(Generator,Lithium)]
           , Set.fromList []]
           
solveAlt :: [Floor]
solveAlt fs = 
  
instance Challenge [Floor] where
  parse _ = floors1
  partOne _ = show $ solve Set.empty (Seq.singleton (0, (0, floors1)))
  partTwo _ = "Kacsa" --show $ solve Set.empty (Seq.singleton (0, (0, floors2)))

