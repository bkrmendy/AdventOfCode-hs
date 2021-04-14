{-# LANGUAGE FlexibleInstances #-}
module Day9 where

import Challenge
import Utils

between :: Char -> Char -> String -> String
between from to = takeWhile (/= to) . drop 1 . dropWhile (/= from)

expand :: String -> (Int, Int, String)
expand str = (letters, times, rest)
  where
    letters = read (between '(' 'x' str) :: Int
    times = read (between 'x' ')' str) :: Int
    rest = drop 1 (dropWhile (/= ')') str)

decodedLength :: String -> Int
decodedLength str =
  case str of
    [] -> 0
    ('(':_) ->
      let
        (letters, times, rest) = expand str
      in letters * times + decodedLength (drop letters rest)
    (_:rest) -> 1 + decodedLength rest

decodedLength2 :: String -> Int
decodedLength2 str =
  case str of
      [] -> 0
      ('(':_) ->
        let (letters, times, rest) = expand str
            repl = take letters rest
         in times * decodedLength2 repl + decodedLength2 (drop letters rest)
      (a:rest) -> 1 + decodedLength2 rest

decode2 :: String -> String
decode2 str =
  case str of
    [] -> []
    ('(':_) ->
      let (letters, times, rest) = expand str
          repl = take letters rest
       in concatRep times (decode2 repl) ++ decode2 (drop letters rest)
    (a:rest) -> a : decode2 rest

instance Challenge String where
  parse = id
  partOne = show . decodedLength
  partTwo = show . decodedLength2

