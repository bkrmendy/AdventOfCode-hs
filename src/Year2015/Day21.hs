module Year2015.Day21 where
import Challenge

data Stats = Stats { hp :: Int, damage :: Int, armor :: Int }
data Equipment = Equipment Int Int Int deriving (Eq)

weapons :: [Equipment]
weapons = [ Equipment 8 4 0
          , Equipment 10 5 0
          , Equipment 25 6 0
          , Equipment 40 7 0
          , Equipment 74 8 0
          ]
          
armors :: [Equipment]
armors  = [ Equipment 13 0 1
          , Equipment 31 0 2
          , Equipment 53 0 3
          , Equipment 75 0 4
          , Equipment 102 0 5
          ] 
          
rings :: [Equipment]
rings = [ Equipment 25 1 0
        , Equipment 50 2 0
        , Equipment 100 3 0
        , Equipment 20 0 1
        , Equipment 40 0 2
        , Equipment 80 0 3
        ]

data Ring = No | Single Equipment | Two (Equipment, Equipment)

combine :: Equipment -> Equipment -> Equipment
combine (Equipment c d a) (Equipment c2 d2 a2) = Equipment (c + c2) (d + d2) (a + a2)

weaponChoices = [w | w <- weapons]
armorChoices = Equipment 0 0 0:armors
ringChoices = Equipment 0 0 0:rings ++ [combine r1 r2 | r1 <- rings, r2 <- rings, r1 /= r2]

mkPlayer :: Stats -> [Equipment] -> (Int, Stats)
mkPlayer (Stats bh bd ba) eqs = (c, Stats bh (bd + d) (ba + a))
  where
    (Equipment c d a) = foldr combine (head eqs) (tail eqs)
    
player :: [(Int, Stats)]
player = [mkPlayer (Stats 100 0 0) [w, a, r] | w <- weaponChoices, a <- armorChoices, r <- ringChoices]

fight :: Stats -> Stats -> Bool
fight (Stats bossHp bossDmg bossArmor) (Stats playerHp playerDmg playerArmor)
  | bossHp <= 0 = True
  | playerHp <= 0 = False
  | otherwise = fight nextBoss nextPlayer
  where
    playerAttack = max 1 (playerDmg - bossArmor)
    bossAttack = max 1 (bossDmg - playerArmor) 
    nextPlayer = Stats (playerHp - bossAttack) playerDmg playerArmor
    nextBoss =  Stats (bossHp - playerAttack) bossDmg bossArmor
  
instance Challenge Stats where
  parse _ = Stats 104 8 1
  partOne boss = show . minimum $ map fst $ filter (fight boss . snd) player
  partTwo boss = show . maximum $ map fst $ filter (not . fight boss . snd) player
