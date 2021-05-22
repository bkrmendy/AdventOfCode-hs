module Year2015.Day11 where
import Challenge
import Data.Char (ord, chr)
import Data.List (group)

increment :: String -> String
increment s = reverse $ incrementI (reverse s)
  where
    incrementI :: String -> String
    incrementI ('z':rest) = 'a':incrementI rest
    incrementI (c:rest)   = chr (ord c + 1):rest
    incrementI []         = "a"

straight :: String -> Bool
straight (a:b:c:rest) = (ord a + 1 == ord b && ord b == ord c - 1) || straight (b:c:rest)
straight _            = False

forbidden :: String -> String -> Bool
forbidden chars = all (`notElem` chars)

different :: String -> Bool
different = (<) 1 . length . filter (> 1) . map length . group

good :: String -> Bool
good str = straight str && forbidden "iol" str && different str

passwords :: String -> [String]
passwords = filter good . iterate increment

solve :: String -> (String, String)
solve pw = (first, second)
  where (first:second:_) = passwords pw

instance Challenge String where
  parse = id
  partOne = fst . solve
  partTwo = snd . solve
