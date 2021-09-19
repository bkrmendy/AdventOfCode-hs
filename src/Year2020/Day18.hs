module Year2020.Day18 where
  
import Challenge

data Token = Number Int
           | Parenthesised [Token]
           | Operator Char
           deriving (Eq, Show)
           
type Tokens = [Token]

data AST = Literal Int
         | Binary (Int -> Int -> Int) AST AST
         | Grouping AST

compile :: Char -> (Int -> Int -> Int)
compile '*' = (*)
compile '+' = (+)
compile chr   = error $ "Unsupported function: " ++ [chr]

evaluate :: AST -> Int
evaluate (Literal n) = n
evaluate (Binary f left right) = f (evaluate left) (evaluate right)
evaluate (Grouping g) = evaluate g

upToNextCloseParen :: String -> (String, String)
upToNextCloseParen = go 0 []
  where go 0 acc (')':rest)  = (reverse acc, rest)
        go n acc (')':rest)  = go (n - 1) (')':acc) rest
        go n acc ('(':rest)   = go (n + 1) ('(':acc) rest
        go n acc (t:rest)           = go n (t:acc) rest

tokenize :: String -> [Token]
tokenize [] = []
tokenize ('(':rest) = Parenthesised (tokenize group):tokenize rest'
  where (group, rest') = upToNextCloseParen rest
tokenize (o:rest) | o == '+' || o == '*' = Operator o:tokenize rest
tokenize (n:rest) | n `elem` ['0'..'9'] = Number (read [n] :: Int):tokenize rest
tokenize (_:rest) = tokenize rest

parseOperand :: Precedence -> [Token] -> (AST, [Token])
parseOperand _    []                      = error "Cannot parse operand: Empty token list"
parseOperand _    (Operator op:_)         = error $ "Cannot parse operand: Operator at head of the token list: " ++ [op] 
parseOperand _    (Number nu:rest)        = (Literal nu, rest)
parseOperand prec (Parenthesised ps:rest) = (parseExpression prec ps, rest)

parsePrattRec :: Int -> Precedence -> AST -> [Token] -> (AST, [Token])
parsePrattRec currentPrec prec left tokens
  | null tokens               = (left, [])
  | precedence < currentPrec  = (left, tokens)
  | otherwise = parsePrattRec currentPrec prec (Binary compiled left next) rest''
  where (Operator op:rest') = tokens
        precedence = prec op
        compiled = compile op
        (next, rest'') = parsePratt precedence prec rest'

parsePratt :: Int -> Precedence -> [Token] -> (AST, [Token])
parsePratt _           _    []      = error "Empty expression!"  
parsePratt currentPrec prec tokens
  | null rest                = (left, [])
  | otherwise                 = parsePrattRec currentPrec prec left rest
  where (left, rest) = parseOperand prec tokens

parseExpression :: Precedence -> [Token] -> AST
parseExpression prec tokens = ast
  where (ast, []) = parsePratt 0 prec tokens

type Precedence = Char -> Int

precPartOne, precPartTwo :: Precedence
precPartOne _   = 1
precPartTwo '+' = 2
precPartTwo _   = 1

solve :: Precedence -> [[Token]] -> Int
solve precedences = sum . map calculate
  where calculate = evaluate . parseExpression precedences
 
instance Challenge [Tokens] where
  parse = map tokenize . lines
  partOne = show . solve precPartOne
  partTwo = show . solve precPartTwo