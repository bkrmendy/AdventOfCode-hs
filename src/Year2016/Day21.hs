{-# LANGUAGE FlexibleInstances #-}
module Day21 where
import Challenge
import Utils
import Data.List (elemIndex, find)
import Text.Parsec hiding (letter)

data Scramble = SwapPosition Int Int
              | SwapLetter Char Char
              | Reverse Int Int
              | Move Int Int
              | RotateLeft Int
              | RotateRight Int
              | RotateBasedOn Char

type Scrambler = String -> String

scramble :: Scramble -> Scrambler
scramble (SwapPosition a b) = swapPosition a b
scramble (RotateBasedOn a)  = rotateBasedOn a
scramble (SwapLetter a b)   = swapLetter a b
scramble (RotateRight a)    = rotateRight a
scramble (RotateLeft a)     = rotateLeft a
scramble (Reverse a b)      = reversee a b
scramble (Move a b)         = move a b

unscramble :: Scramble -> String -> String
unscramble (SwapPosition a b) xs  = scramble (SwapPosition b a) xs
unscramble (RotateBasedOn a) xs   = flip rotateLeft xs $ head [i | i <- [0..]
                                                                 , xs == (rotateBasedOn a $ rotateLeft i xs)]
unscramble (SwapLetter a b) xs    = scramble (SwapLetter b a) xs
unscramble (RotateRight a) xs     = scramble (RotateLeft a) xs
unscramble (RotateLeft a) xs      = scramble (RotateRight a) xs
unscramble (Reverse a b) xs       = scramble (Reverse a b) xs
unscramble (Move a b) xs          = scramble (Move b a) xs

swapPosition :: Int -> Int -> String -> String
swapPosition x y xs = replace x b $ replace y a xs
  where
    a = xs !! x
    b = xs !! y

swapLetter :: Char -> Char -> String -> String
swapLetter a b xs = replace x b $ replace y a xs
  where
    x = unsafeFromMaybe (elemIndex a xs)
    y = unsafeFromMaybe (elemIndex b xs)

reversee :: Int -> Int -> String -> String
reversee from to xs = take from xs ++ reversed ++ drop (to + 1) xs
  where
    reversed = reverse (take (to - from + 1) $ drop from xs)

move :: Int -> Int -> String -> String
move from to xs = insert letter to $ remove from xs
  where
    letter = xs !! from

rotateLeft :: Int -> String -> String
rotateLeft n xs = drop n' xs ++ take n' xs
  where
    n' = n `mod` length xs

rotateRight :: Int -> String -> String
rotateRight n xs = drop l xs ++ take l xs
  where
    n' = n `mod` length xs
    l = length xs - n'

rotateBasedOn :: Char -> String -> String
rotateBasedOn letter xs = rotateRight amount xs
  where
    ix = unsafeFromMaybe (elemIndex letter xs)
    amount = 1 + ix + (if ix >= 4 then 1 else 0)

pSwapPosition = SwapPosition <$> (string "swap position " *> int) <*> (string " with position "*> int)
pSwapLetter = SwapLetter <$> (string "swap letter " *> anyChar) <*> (string " with letter " *> anyChar)
pRotateLeft  = RotateLeft <$> (string "rotate left " *> int <* string " steps")
pRotateRight  = RotateRight <$> (string "rotate right " *> int <* string " steps")
pRotateBased = RotateBasedOn <$> (string "rotate based on position of letter " *> anyChar)
pReverse = Reverse <$> (string "reverse positions " *> int) <*> (string " through " *> int)
pMove = Move <$> (string "move position " *> int) <*> (string " to position " *> int)

parseInstr :: Parsec String () Scramble
parseInstr = try pSwapPosition
              <|> try pSwapLetter
              <|> try pRotateLeft
              <|> try pRotateRight
              <|> try pRotateBased
              <|> try pReverse
              <|>pMove

instance Challenge [Scramble] where
  parse = parseLines (sepBy1 parseInstr newline)
  partOne = show . foldl (flip ($)) "abcdefgh" . map scramble
  partTwo = show . foldl (flip ($)) "fbgdceah" . map unscramble . reverse