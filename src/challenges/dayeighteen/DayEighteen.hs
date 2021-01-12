module DayEighteen where
import Challenge

newtype Row = Row { unRow :: String } deriving (Show)

room :: Row -> [Row]
room row = row:nextRows
  where
    nextRows :: [Row]
    nextRows = room $ Row $ generate $ "." ++ unRow row ++ "."

    generate :: String -> String
    generate r@('^':'^':'.':_) = '^':generate (drop 1 r)
    generate r@('.':'^':'^':_) = '^':generate (drop 1 r)
    generate r@('^':'.':'.':_) = '^':generate (drop 1 r)
    generate r@('.':'.':'^':_) = '^':generate (drop 1 r)
    generate r@(_:_:_:_) = '.':generate (drop 1 r)
    generate _ = []

solve :: Int -> Row -> Int
solve n = sum . map (length . filter (== '.') . unRow) . take n . room

instance Challenge Row where
  parse = Row
  partOne = show . solve 40
  partTwo = show . solve 400000