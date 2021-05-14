module Year2015.Day8 where
import Challenge
import Control.Monad.State

data Encoded
  = Quoted [Encoded]
  | Escaped Char
  | Hex (Char, Char)
  | Bare Char

len :: Encoded -> Int
len (Quoted   s)  = sum (map len s)
len (Escaped  _)  = 1
len (Hex      _)  = 1
len (Bare     _)  = 1

quoted :: State String [Encoded]
quoted = do
  str <- get
  case str of
    ('"':rest) -> put rest >> return []
    _ -> decode

decode :: State String [Encoded]
decode = do
  str <- get
  case str of
    ('\\':'x':a:b:rest) -> put rest >> (:) (Hex (a, b)) <$> decode
    ('\\':a:rest)       -> put rest >> (:) (Escaped a) <$> decode
    ('"':rest)          -> put rest >> (Quoted <$> quoted) >>= \q -> (q:) <$> decode
    (a:rest)            -> put rest >> (:) (Bare a) <$> decode
    []                  -> return []

encode :: String -> String
encode []           = []
encode ('"':rest)   = ['\\', '\"'] <> encode rest
encode ('\\':rest)  = ['\\', '\\'] <> encode rest
encode (a:rest)     = a:encode rest

toLiteral :: String -> String
toLiteral literal = ['"'] <> encode literal <> ['"']

solve :: String -> Int
solve literal = charsInMemory - decodedChars
  where
    charsInMemory = length literal
    decodedChars = sum (map len $ evalState decode literal)

solve2 :: String -> Int
solve2 literal = length (toLiteral literal) - length literal

instance Challenge [String] where
  parse = lines
  partOne = show . sum . map solve
  partTwo = show . sum . map solve2