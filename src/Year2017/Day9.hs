module Year2017.Day9 where
import Challenge
import Utils (parseLines)

import Text.Parsec as P

data Piece
  = Garbage [Piece]
  | Group [Piece]
  | Chr Char
  | Cancelled
  deriving (Show)


pCancel :: Parsec String () Piece
pCancel = Cancelled <$ (char '!' *> anyChar)

pGarbageChar :: Parsec String () Piece
pGarbageChar = Chr <$> (letter <|> space <|> char '{' <|> char '}' <|> char ',' <|> char '<' <|> char '\\' <|> char '\"' <|> char '\'')

pGarbage :: Parsec String () Piece
pGarbage = Garbage <$> between (char '<') (char '>') (option [] (many1 (pCancel <|> pGarbageChar)))

pGroup :: Parsec String () Piece
pGroup = Group <$> between (char '{') (char '}') (option [] (sepBy1 pPiece (char ',')))

pPiece :: Parsec String () Piece
pPiece = pGroup <|> pGarbage

score :: Int -> Piece -> Int
score acc (Group pcs) = acc + sum (map (score (acc + 1)) pcs)
score _   _           = 0

garbage :: Piece -> Int
garbage Cancelled       = 0
garbage (Chr _)         = 1
garbage (Garbage chars) = sum $ map garbage chars
garbage (Group pcs) = sum $ map garbage pcs

instance Challenge Piece where
  parse = parseLines pPiece
  partOne = show . score 1
  partTwo = show . garbage



