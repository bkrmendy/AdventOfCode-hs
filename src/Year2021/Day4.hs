module Year2021.Day4 where

import Challenge
import Utils (readInt, transpose)
  
import Data.List (partition)
import Data.List.Split (splitOn)

type Board = [[(Bool, Int)]]
data Bingo = MkBingo { _numbers :: [Int], _boards :: [Board] }

parseBoard :: String -> Board
parseBoard board = [ line l | l <- "\n" `splitOn` board]
  where line ns = map (\i -> (False, readInt i))
                $ filter (not . null)
                $ " " `splitOn` ns
  
parseI :: String -> Bingo
parseI input = MkBingo numbers boards
  where (ns:bs) = "\n\n" `splitOn` input
        numbers = map readInt $ "," `splitOn` ns
        boards = map parseBoard bs
  
mark :: Int -> Board -> Board
mark n board = [ map markI l | l <- board]
  where markI (marked, field) = if field == n then (True, field) else (marked, field)
  
wins :: Board -> Bool
wins board = rowsWin || columnsWin
  where condition = any (all fst)
        rowsWin = condition board
        columnsWin = condition $ transpose board
        
score :: Int -> Board -> Int
score n board = (*) n
              $ sum
              $ [field | line <- board, (False, field) <- line]

playPartOne, playPartTwo :: Bingo -> (Int, Board)
playPartOne (MkBingo [] _) = error "Out of numbers!"
playPartOne (MkBingo (n:ns) boards) = case filter wins nextBoards of
  [] -> playPartOne $ MkBingo ns nextBoards
  (b:_) -> (n, b)
  where nextBoards = map (mark n) boards

playPartTwo (MkBingo [] _) = error "Out of numbers!"
playPartTwo (MkBingo (n:ns) boards) = case partition wins nextBoards of
  (ws, []) -> (n, last ws)
  (_, bs) -> playPartTwo $ MkBingo ns bs
  where nextBoards = map (mark n) boards
 
partOneI, partTwoI :: Bingo -> Int
partOneI = uncurry score . playPartOne
partTwoI = uncurry score . playPartTwo

instance Challenge Bingo where
  parse = parseI
  partOne = show . partOneI
  partTwo = show . partTwoI