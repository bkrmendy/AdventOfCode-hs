module DayFourteen where

import Challenge
import Data.Word8
import qualified Data.ByteString as Bs
import qualified Data.ByteString.UTF8 as BLU
import Utils

newtype Salt = Salt { unSalt :: Bs.ByteString }

keyStretchHash :: Int -> Bs.ByteString -> Bs.ByteString
keyStretchHash 0 str = str
keyStretchHash n str = keyStretchHash (n - 1) (md5 str)

mush :: Salt -> Int -> Bs.ByteString
mush (Salt s) = Bs.append s . BLU.fromString . show

hashes :: (Bs.ByteString -> Bs.ByteString) -> Salt -> [Bs.ByteString]
hashes f salt = [ f $ mush salt i | i <- [0..]]

hasThree :: (Eq a) =>  [a] -> Maybe a
hasThree elems =
  case elems of
     (a:b:c:rest) -> if a == b && b == c then Just a else hasThree (b:c:rest)
     _ -> Nothing

hasFiveOf :: Word8 -> [Bs.ByteString] -> Bool
hasFiveOf letter = any (Bs.isInfixOf (Bs.replicate 5 letter))

solve :: Int -> [Bs.ByteString] -> [Int]
solve index futureHashes =
  case hasThree (Bs.unpack thisHash) of
    Nothing -> next
    Just letter ->
      if hasFiveOf letter (tail hashRange) then index:next
      else next
  where
    hashRange = take 1001 futureHashes
    thisHash = head hashRange
    next = solve (index + 1) (drop 1 futureHashes)

instance Challenge Salt where
    parse = Salt . BLU.fromString
    partOne = show . last . take 64 . solve 0 . hashes (keyStretchHash 1)
    partTwo = show . last . take 64 . solve 0 . hashes (keyStretchHash 2017)