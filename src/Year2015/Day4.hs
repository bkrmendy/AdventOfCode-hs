{-# LANGUAGE FlexibleInstances #-}
module Year2015.Day4 where
import Challenge
import Utils (md5)
import qualified Data.ByteString as Bs
import qualified Data.ByteString.UTF8 as BSU

type Seed = Bs.ByteString

zeros :: Int -> Bs.ByteString -> Bool
zeros n str = Bs.take n str == BSU.fromString (replicate n '0')

mine :: (Bs.ByteString -> Bool) -> Int -> Bs.ByteString -> Int
mine ok n seed
  | ok (md5 $ Bs.concat [seed, BSU.fromString (show n)]) = n
  | otherwise = mine ok (n + 1) seed

instance Challenge Seed where
  parse = BSU.fromString
  partOne = show . mine (zeros 5) 0
  partTwo = show . mine (zeros 6) 0
  

