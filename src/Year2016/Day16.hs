module Day16 where

import Challenge

-- ^ due to https://github.com/ttencate/aoc2016/blob/master/16_haskell/16b.hs
import qualified Data.ByteString.Char8 as C

newtype Binary = Binary { unBinary :: String }

flipBit :: Char -> Char
flipBit '0' = '1'
flipBit '1' = '0'
flipBit _ = error "Non-binary digit found!"

checksum :: C.ByteString -> C.ByteString
checksum hash
  | odd $ C.length hash = hash
  | otherwise = checksum' (C.unpack hash) ""
  where
    digit a b
      | a == b = '1'
      | otherwise = '0'
    checksum' h acc =
       case h of
         (a:b:rest) -> checksum' rest (digit a b:acc)
         [] -> checksum (C.reverse $ C.pack acc)

generate ::  C.ByteString -> [C.ByteString]
generate from = result:generate result
  where
    a = from
    b = C.map flipBit $ C.reverse a
    result = C.concat [a, C.pack "0", b]
    
solve :: Int -> Binary -> String
solve n = show . checksum . C.take n . head . dropWhile (\hash -> C.length hash < n) . generate . C.pack . unBinary

instance Challenge Binary where
  parse = Binary
  partOne = solve 272
  partTwo = solve 35651584