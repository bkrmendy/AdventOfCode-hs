module Year2021.Day17 where
  
import Challenge

data Bounds = MkBounds (Int, Int) (Int, Int) 

data Probe = MkProbe { _position :: (Int, Int), _velocity :: (Int, Int) }

inBounds :: Bounds -> Probe -> Bool
inBounds (MkBounds (minX, maxX) (minY, maxY)) (MkProbe (px, py) _) = px <= maxX && px >= minX && py >= minY && py <= maxY

sling :: Bounds -> Probe -> Maybe [Probe]
sling b@(MkBounds (minX, maxX) (minY, maxY)) probe@(MkProbe (px, py) (vx, vy))
  | px > maxX || py < minY = Nothing
  | inBounds b probe = Just [probe]
  | otherwise = (nextProbe:) <$> sling b nextProbe 
  where nextProbe = MkProbe (px + vx, py + vy) (max 0 (vx - 1), vy - 1)

run :: Bounds -> [Int]
run b@(MkBounds (minX, maxX) (minY, maxY)) = do
  vx <- [0..maxX]
  vy <- [minY .. -minY]
  case sling b (MkProbe (0, 0) (vx, vy)) of
    Nothing -> mempty
    Just ps -> pure $ maximum $ map (snd . _position) ps
  
partOneI :: Bounds -> Int
partOneI = maximum . run

partTwoI :: Bounds -> Int
partTwoI = length . run 
  
instance Challenge Bounds where
  parse = const $ MkBounds (137, 171) (-98, -73)
  partOne = show . partOneI
  partTwo = show . partTwoI
  


