module Day10 where

import Challenge
import Data.Map
import Data.Function
import Text.Parsec as Parsec
import Utils hiding (insert)

data Destination
  = Bot Int
  | Output Int

data InputLine
  -- ^ bot 2 gives low to bot 1 and high to bot 0
  = Rule Int Destination Destination
  -- ^ value 3 goes to bot 1
  | GoesTo Int Int

parseBot = Bot <$> (Parsec.string "bot " *> int)
parseOutput = Output <$> (Parsec.string "output " *> int)

parseDestination = parseBot <|> parseOutput

parseRule = Rule  <$> (Parsec.string "bot " *> int)
                  <*> (Parsec.string " gives low to " *> parseDestination)
                  <*> (Parsec.string " and high to " *> parseDestination)

parseEvent = GoesTo  <$> (Parsec.string "value " *> int)
                     <*> (Parsec.string " goes to bot " *> int)

data BotState
  = ZeroValues
  | OneValue Int

data Estate
  = Estate
  { bots :: Map Int BotState
  , rules :: Map Int (Destination, Destination)
  , outputs :: Map Int Int
  , specialBot :: Int
  }

cascade :: Estate -> Int -> Int -> Estate
cascade estate value bot =
  case state of
    ZeroValues -> estate { bots = insert bot (OneValue value) (bots estate) }
    OneValue pValue ->
      case rule of
          (Bot n, Bot m) ->
            (nextEstate pValue)
            & \nn -> cascade nn (min pValue value) n
            & \nn -> cascade nn (max pValue value) m
          (Output n, Output m) ->
            nextEstate pValue
            & \nn -> nn { outputs = insert n (min pValue value) (outputs nn) }
            & \nn -> nn { outputs = insert m (max pValue value) (outputs nn) }
          (Bot n, Output m) ->
            nextEstate pValue
            & \nn ->nn { outputs = insert m (max pValue value) (outputs nn) }
            & \nn -> cascade nn (min pValue value) n
          (Output n, Bot m) ->
            nextEstate pValue
            & \ nn -> nn { outputs = insert n (min pValue value) (outputs nn) }
            & \ne -> cascade ne (max pValue value) m
  where
    state = Data.Map.findWithDefault ZeroValues bot (bots estate)
    rule = Data.Map.findWithDefault (error $ show bot ++ " not in db") bot (rules estate)
    nextEstate pValue = estate
            { bots = insert bot ZeroValues (bots estate),
              specialBot = newSpecial pValue
            }
    newSpecial pValue =
      if (pValue == 61 && value == 17) || (pValue == 17 && value == 61)
      then bot
      else specialBot estate


run :: Estate -> [(Int, Int)] -> Estate
run estate commands =
  case commands of
    [] -> estate
    (value, bot):rest -> cascade estate value bot & \e -> run e rest

solve lines
  = run Estate { bots = fromList []
               , rules = fromList [(i, (hi, lo)) | (Rule i hi lo) <- lines]
               , outputs = fromList []
               , specialBot = 0
               }
         [(value, to) | (GoesTo value to) <- lines]

extract :: Estate -> Int
extract estate = zero * one * two
  where
    outs = outputs estate
    zero = findWithDefault undefined 0 outs
    one = findWithDefault undefined 1 outs
    two = findWithDefault undefined 2 outs


instance Challenge [InputLine] where
  parse = parseLines (Parsec.sepBy1 (parseRule <|> parseEvent) Parsec.newline)
  partOne = show . specialBot . solve
  partTwo = show . extract . solve