{-# LANGUAGE FlexibleInstances #-}
module DayEight where
import Challenge
import Utils

import Text.Parsec as Parsec

type Display = [String]
type Command = Display -> Display

activate :: Int -> Int -> Display -> Display
activate wide tall display =
  litRows ++ drop tall display
  where
    lightUp row = replicate wide '#' ++ drop wide row
    litRows = map lightUp $ take tall display

rotateColumn :: Int -> Int -> Display ->  Display
rotateColumn column by display = map transform (zip display [0..])
  where
    transform :: (String, Int) -> String
    transform (c, i) =
      let prev = (display !! ((i - by) `mod` height)) !! column
      in replace column prev c

rotateRow :: Int -> Int -> Display ->  Display
rotateRow row by display = map transform (zip display [0..])
  where
    shift = (width - by) `mod` width
    transform :: (String, Int) -> String
    transform (r, i)
       | i == row = drop shift r ++ take shift r
       | otherwise = r

parseInt :: Parsec.Parsec String () Int
parseInt = read <$> Parsec.many1 Parsec.digit

parseRectCommand = activate <$> (Parsec.string "rect " *> parseInt) <*> (Parsec.string "x" *> parseInt)
parseRowCommand = rotateRow <$> (Parsec.string "rotate row y=" *> parseInt) <*> (Parsec.string " by " *> parseInt)
parseColumnCommand = rotateColumn <$> (Parsec.string "rotate column x=" *> parseInt) <*> (Parsec.string " by " *> parseInt)

commandParser :: Parsec.Parsec String () [Command]
commandParser =
  Parsec.sepBy1 ( Parsec.try parseRectCommand
                  <|> Parsec.try  parseRowCommand
                  <|> Parsec.try parseColumnCommand
  ) Parsec.newline

parseCommands :: String -> [Command]
parseCommands = parse
  where
    parse input =
      case Parsec.parse commandParser "" input of
        Left err -> error $ show err
        Right commands -> commands

width = 50
height = 6

mkDisplay :: Display
mkDisplay = replicate height $ replicate width '.'

nPixelsLit :: Display -> Int
nPixelsLit display = sum $ map countPixelsOn display
  where
    countPixelsOn row = length $ filter (== '#') row

runCommands :: Display -> [Command] ->  Display
runCommands = foldl (flip ($))

instance Challenge [Command] where
  parse = parseCommands
  partOne = show . nPixelsLit . runCommands mkDisplay
  partTwo = unlines . runCommands mkDisplay