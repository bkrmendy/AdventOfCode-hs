module Year2017.Day20 where
import            Challenge
import            Utils (int, parseL)

import            Data.List (sortOn, groupBy)
import            Text.Parsec

data Particle = Particle {_pos :: (Int, Int, Int), _vel :: (Int, Int, Int), _acc :: (Int, Int, Int) } deriving (Eq, Ord)

pTriple :: Parsec String () (Int, Int, Int)
pTriple = (,,) <$> (char '<' *> int) <*> (char ',' *> int) <*> (char ',' *> int <* char '>')

pParticle :: Parsec String () Particle
pParticle = Particle <$> (string "p=" *> pTriple) <*> (string ", v=" *> pTriple) <*> (string ", a=" *> pTriple)

manhattan :: Particle -> Int
manhattan (Particle (a, b, c) _ _) = abs a + abs b + abs c

step :: Particle -> Particle
step (Particle (pa, pb, pc) (va, vb, vc) (aa, ab, ac)) = Particle (pa + nva, pb + nvb, pc + nvc) (nva, nvb, nvc) (aa, ab, ac)
  where (nva, nvb, nvc) = (va + aa, vb + ab, vc + ac)

pt1 :: Int -> [Particle] -> Int
pt1 gens ps = fst . head . sortOn snd $ zip [0..] (map manhattan longTerm)
  where longTerm = iterate (map step) ps !! gens

move :: [Particle] -> [Particle]
move = concat . filter ((==) 1 . length) . groupBy posSame . sortOn pos . map step
  where
    pos (Particle p _ _) = p
    posSame (Particle p1 _ _) (Particle p2 _ _) = p1 == p2

pt2 :: Int -> [Particle] -> Int
pt2 gens ps = length $ iterate move ps !! gens

instance Challenge [Particle] where
  parse = parseL pParticle
  partOne = show . pt1 1000
  partTwo = show . pt2 20000