module Year2015.Day12 where
import Challenge
import Utils (int)

import Text.Parsec as Parsec

-- Advent Object Notation
data AON
  = AString String
  | ANumber Int
  | Array [AON]
  | Object [(String, AON)]

quoted :: Parsec String () String
quoted = between (char '"') (char '"') (many1 letter)

astring :: Parsec String () AON
astring = AString <$> quoted

anumber :: Parsec String () AON
anumber = ANumber <$> int

array :: Parsec String () AON
array = Array <$> between (char '[') (char ']') (sepBy1 aon (char ','))

kvPair :: Parsec String () (String, AON)
kvPair = (,) <$> quoted <*> (char ':' *> aon)

object :: Parsec String () AON
object = Object <$> between (char '{') (char '}') (sepBy1 kvPair (char ','))

aon :: Parsec String () AON
aon = astring <|> anumber <|> array <|> object

parseI :: String -> AON
parseI str
  = case Parsec.parse aon "" str of
      Right val -> val
      Left err -> error (show err)

walk :: (AON -> Bool) -> Int -> AON -> Int
walk _ acc (ANumber n)              = n + acc
walk _ acc (AString _)              = acc
walk _ acc (Array [])               = acc
walk f acc (Array (a:s))            = walk f (walk f acc a) (Array s)
walk _ acc (Object [])              = acc
walk f acc o@(Object ((_, v):kvs))    =
  if f o
    then walk f (walk f acc v) (Object kvs)
    else acc

red :: AON -> Bool
red (Object kvs)    = null [ k | (k, (AString "red")) <- kvs]
red _               = False

instance Challenge AON where
  parse = parseI
  partOne = show . walk (const True) 0
  partTwo = show . walk red 0
