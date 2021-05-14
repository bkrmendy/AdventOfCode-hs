module Day15 where

import qualified Text.Parsec as P
import Utils

import Challenge

data DiskDescription = DiskDescription { nPositions :: Integer
                                       , startingPosition :: Integer
                                       }

parseDisk = DiskDescription <$> (toInteger <$>  (P.string "Disc #" *> int *> P.string " has " *> int <* P.string " positions; at time=0, it is at position "))
                            <*> (toInteger <$> (int <* P.char '.'))

residii :: [DiskDescription] -> [Integer]
residii descs = swizzled
  where
    res = map (negate . startingPosition) descs
    swizzled = map (\(r, c) -> r - c) (zip res [1..])

modulii :: [DiskDescription] -> [Integer]
modulii = map nPositions

solve i = unsafeFromMaybe $ chineseRemainder (residii i) (modulii i)

instance Challenge [DiskDescription] where
  parse = parseLines (P.sepBy1 parseDisk P.newline)
  partOne = show . solve
  partTwo = show . solve . \discs -> discs ++ [DiskDescription 11 0]