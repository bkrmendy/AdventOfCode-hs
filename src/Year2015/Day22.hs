module Year2015.Day22 where
import Challenge
import Utils (replace)
import Data.Maybe (catMaybes)
import Data.List (sortOn)

data Effect = Shield | Poison | Recharge deriving (Eq)
data Winner = Player | Boss deriving (Eq, Ord)
data GameState = GameState { playerHealth :: Int
                           , bossHealth :: Int
                           , bossDamage :: Int
                           , mana :: Int
                           , pEffects :: [(Int, Effect)]
                           }

decreaseEffectTimers :: GameState -> GameState
decreaseEffectTimers state = state { pEffects = [(t - 1, e) | (t, e) <- pEffects state] }

applyEffects :: GameState -> GameState
applyEffects state = foldr doEffect state activeEffects
  where
    activeEffects = [e | (t, e) <- pEffects state, t >= 0]
    doEffect :: Effect -> GameState ->  GameState
    doEffect Poison st = st { bossHealth = bossHealth st - 3 }
    doEffect Recharge st = st { mana = mana st + 101 }
    doEffect Shield st = st

bossDealsDamage :: GameState -> GameState
bossDealsDamage state = state { playerHealth = playerHealth state - actualBossDamage}
  where
    actualBossDamage = bossDamage state - playerArmor
    playerArmor = sum [7 | (t, Shield) <- pEffects state, t >= 1]

actions :: [(Int, GameState -> GameState)]
actions = [ (53, missile)
          , (73, drain)
          ]

effects :: [(Int, Int, Effect)]
effects = [ (113, 6, Shield)
          , (173, 6, Poison)
          , (229, 5, Recharge)
          ]

cast :: Int -> GameState -> (Int, Int, Effect) -> [(Int, GameState)]
cast manaUsed state (cost, duration, effect) = [(manaUsed + cost, newState) | effect `elem` possibleEffects
                                                                            , mana state >= cost]
  where
    possibleEffects = [e | (t, e) <- pEffects state, t <= 0]
    activate (d, e) (du, ef) = if e == ef then (d, e) else (du, ef)
    newState = state { pEffects = map (activate (duration, effect)) (pEffects state)
                     , mana = mana state - cost
                     }

missile :: GameState -> GameState
missile state = state { bossHealth = bossHealth state - 4
                       , mana = mana state - 53
                       }

drain :: GameState -> GameState
drain state = state { bossHealth = bossHealth state - 2
                    , playerHealth = playerHealth state + 2
                    , mana = mana state - 73
                    }

doAction :: Int -> GameState -> (Int, GameState -> GameState) -> [(Int, GameState)]
doAction manaUsed state (cost, action) = [(manaUsed + cost, action state) | mana state >= cost]

bossTurn :: GameState -> GameState
bossTurn = bossDealsDamage . decreaseEffectTimers . applyEffects

decreasePlayerHealth :: GameState -> GameState
decreasePlayerHealth state = state { playerHealth = playerHealth state - 1 }

type Difficulty = GameState -> GameState
easy :: Difficulty
easy = applyEffects . decreaseEffectTimers
hard :: Difficulty
hard = applyEffects . decreaseEffectTimers . decreasePlayerHealth

playerTurn :: Int -> Difficulty -> GameState -> [Maybe Int]
playerTurn manaUsed difficulty state
  | bossHealth state <= 0 = [Just manaUsed]
  | playerHealth state <= 0 || mana state <= 0 = [Nothing]
  | otherwise = concat [playerTurn m difficulty (bossTurn s) | (m, s) <- sortOn fst possibilities]
  where
    nextState = difficulty state
    possibilities = concatMap (doAction manaUsed nextState) actions ++ concatMap (cast manaUsed nextState) effects

easyDiff :: GameState -> GameState
easyDiff = applyEffects . decreaseEffectTimers

startState :: GameState
startState = GameState { playerHealth = 50
                       , mana = 500
                       , bossHealth = 71
                       , bossDamage = 10
                       , pEffects = [(-1, Shield), (-1, Poison), (-1, Recharge)]
                       }

startStateEx :: GameState
startStateEx = GameState { playerHealth = 10
                         , mana = 250
                         , bossHealth = 14
                         , bossDamage = 8
                         , pEffects = [(-1, Shield), (-1, Poison), (-1, Recharge)]
                         }

-- ^ TODO: refactor to use state monad
-- part one not ok

solve difficulty = show . head . take 1 . catMaybes . playerTurn 0 difficulty 

instance Challenge GameState where
  parse _ = startState
  partOne = solve easy
  partTwo = solve hard
