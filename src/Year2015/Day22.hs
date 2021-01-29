module Year2015.Day22 where
import Challenge
import Data.Maybe (catMaybes)
import Data.List (sortOn)

data Effect = Shield | Poison | Recharge deriving (Eq)
data Winner = Player | Boss deriving (Eq, Ord)
data GameState = GameState { playerHealth :: Int
                           , bossHealth :: Int
                           , bossDamage :: Int
                           , mana :: Int
                           , activeEffects :: [(Int, Effect)]
                           , passiveEffects :: [Effect]
                           }

decreaseEffectTimers :: GameState -> GameState
decreaseEffectTimers state = state { activeEffects = nextActiveEffects, passiveEffects = nextPassiveEffects }
  where
    nextActiveEffects = [(t - 1, e) | (t, e) <- activeEffects state]
    nextPassiveEffects = passiveEffects state ++ [e | (t, e) <- activeEffects state, t <= 0]

applyEffects :: GameState -> GameState
applyEffects state = foldr (doEffect . snd) state (activeEffects state)
  where
    doEffect :: Effect -> GameState ->  GameState
    doEffect Poison st= st { bossHealth = bossHealth st - 3 }
    doEffect Recharge st = st { mana = mana st + 101 }
    doEffect Shield st = st

bossDealsDamage :: GameState -> GameState
bossDealsDamage state = state { playerHealth = playerHealth state - actualBossDamage}
  where
    actualBossDamage = max 1 (bossDamage state - playerArmor)
    playerArmor = sum [7 | (_, Shield) <- activeEffects state]

actions :: [(Int, GameState -> GameState)]
actions = [ (53, missile)
          , (73, drain)
          ]

effects :: [(Int, Int, Effect)]
effects = [ (113, 6, Shield)
          , (173, 6, Poison)
          , (229, 5, Recharge)
          ]

-- 229 + 113 + 73 + 173 + 53

cast :: Int -> GameState -> (Int, Int, Effect) -> [(Int, GameState)]
cast manaUsed state (cost, duration, effect) = [(manaUsed + cost, newState) | effect `elem` passiveEffects state
                                                                            , mana state >= cost]
  where
    newState = state { activeEffects = (duration, effect):activeEffects state
                     , passiveEffects = filter (effect /=) (passiveEffects state)
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
bossTurn = bossDealsDamage . applyEffects . decreaseEffectTimers

playerTurn :: Int -> GameState -> [Maybe Int]
playerTurn manaUsed state
  | bossHealth state <= 0 = [Just manaUsed]
  | playerHealth state <= 0 || mana state <= 0 = [Nothing]
  | otherwise = concat [playerTurn m (bossTurn s) | (m, s) <- sortOn fst possibilities]
  where
    nextState = applyEffects (decreaseEffectTimers state)
    possibilities = concatMap (doAction manaUsed nextState) actions ++ concatMap (cast manaUsed nextState) effects

startState :: GameState
startState = GameState { playerHealth = 50
                       , mana = 500
                       , bossHealth = 71
                       , bossDamage = 10
                       , activeEffects = []
                       , passiveEffects = [Shield, Poison, Recharge]
                       }

startStateEx :: GameState
startStateEx = GameState { playerHealth = 10
                         , bossHealth = 14
                         , bossDamage = 8
                         , mana = 250
                         , activeEffects = []
                         , passiveEffects = [Shield, Poison, Recharge]
                         }

instance Challenge GameState where
  parse _ = startState
  partOne = show . head . take 1 . catMaybes . playerTurn 0
  partTwo _ = "2"
