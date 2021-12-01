module Year2021.Day1 where
  
import Utils (readInt)
  
import Challenge

partOneI :: [Int] -> Int
partOneI ns = length
            $ filter (uncurry (<))
            $ zip ns (tail ns)

windows :: Int -> [a] -> [[a]]
windows n ns | length ns < n = [ns]
windows n ns@(_:rest)        = take n ns : windows n rest 

partTwoI :: [Int] -> Int
partTwoI = partOneI
         . map sum
         . windows 3

instance Challenge [Int] where
  parse = map readInt . lines
  partOne = show . partOneI
  partTwo = show . partTwoI

