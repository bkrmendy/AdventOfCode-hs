module Year2020.Day4 where

import Challenge
import Utils (int, parseLines)

import Text.Parsec as P
import Data.List.Split (splitOn)
import Data.Char (digitToInt)
import Data.Either (isRight)

data Value
  = Year Int
  | Height Int
  | Color String
  | EyeColor String
  | Pid String

type Entry = (String, String)
type Passport = [Entry]

toInt :: [Int] -> Int
toInt = foldl (\a v -> a * 10 + v) 0

year :: (Int -> Bool) -> Parsec String () Value
year v = do
  y <- toInt . map digitToInt <$> count 4 digit
  if v y
    then return $ Year y
    else fail "invalid"


height :: (String -> Int -> Bool) -> Parsec String () Value
height v = do
  is <- int
  m <- many1 letter
  if v m is
    then return (Height is)
    else fail "invalid"

colorLetter :: Parsec String () Char
colorLetter = choice $ map char $ ['0'..'9'] ++ ['a'..'f']

hairColor :: Parsec String () Value
hairColor = Color <$> (char '#' *> count 6 colorLetter)

eyeColors :: [String]
eyeColors = ["amb", "blu", "brn", "gry", "grn", "hzl", "oth"]

eyeColor :: Parsec String () Value
eyeColor = do
  col <- many1 letter
  if col `elem` eyeColors
    then return (EyeColor col)
    else fail "invalid"

pid :: Parsec String () Value
pid = Pid <$> count 9 digit

entryChar :: Parsec String () Char
entryChar = char '#' <|> letter <|> digit

ws :: Parsec String () Char
ws = space <|> newline

entry :: Parsec String () (String, String)
entry = (,) <$> (many1 letter <* char ':') <*> many1 entryChar

passport :: Parsec String () Passport
passport = sepBy1 entry ws

parseI :: String -> [Passport]
parseI = map (parseLines passport) . splitOn "\n\n"

expected :: [String]
expected = ["byr", "iyr", "eyr", "hgt", "hcl", "ecl", "pid"]

allFieldsPresent :: Passport -> Bool
allFieldsPresent es = all (`elem` keys) expected
  where keys = map fst es

validHeight :: String -> Int -> Bool
validHeight "cm" h = 150 <= h && h <= 193
validHeight "in" h = 59 <= h && h <= 76
validHeight _    _ = False

validP :: String -> Parsec String () Value -> Bool
validP s p = isRight $ P.parse p "" s

fieldSemanticallyValid :: Entry -> Bool
fieldSemanticallyValid ("byr", e) = validP e (year (\y -> 1920 <= y && y <= 2002))
fieldSemanticallyValid ("iyr", e) = validP e (year (\y -> 2010 <= y && y <= 2020))
fieldSemanticallyValid ("eyr", e) = validP e (year (\y -> 2020 <= y && y <= 2030))
fieldSemanticallyValid ("hgt", e) = validP e (height validHeight)
fieldSemanticallyValid ("hcl", e) = validP e hairColor
fieldSemanticallyValid ("ecl", e) = validP e eyeColor
fieldSemanticallyValid ("pid", e) = validP e pid
fieldSemanticallyValid _          = True

instance Challenge [Passport] where
  parse = parseI
  partOne = show . length . filter allFieldsPresent
  partTwo = show . length . filter (\pass -> allFieldsPresent pass && all fieldSemanticallyValid pass)
