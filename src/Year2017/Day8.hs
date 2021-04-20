module Year2017.Day8 where
import              Challenge
import              Utils (word, int, parseL)

import              Text.Parsec hiding (State)
import qualified    Data.Map as M
import              Control.Monad.State

data Machine = Machine { _regs :: M.Map String Int, _largest :: Int }

type Operation = Int -> Int -> Int
type Predicate = Int -> Int -> Bool

pOp :: Parsec String () Operation
pOp = ((+) <$ string "inc") <|> ((-) <$ string "dec")

pPred :: Parsec String () Predicate
pPred = try ((==) <$ string "==")
        <|> ((/=) <$ string "!=")
        <|> try ((>=) <$ string ">=")
        <|> ((>) <$ string ">")
        <|> try ((<=) <$ string "<=")
        <|> ((<) <$ string "<")

pClause :: Parsec String () Clause
pClause = Clause <$> word
                 <*> (space *> pOp)
                 <*> (space *> int)
                 <*> (string " if " *> word)
                 <*> (space *> pPred)
                 <*> (space *> int)

data Clause = Clause String Operation Int String Predicate Int

interpret :: Clause -> State Machine ()
interpret (Clause oReg op oConst pReg p pConst) = do
  pVal <- gets $ M.findWithDefault 0 pReg . _regs
  when (p pVal pConst) $ do
       oVal <- gets $ M.findWithDefault 0 oReg . _regs
       modify' $ \(Machine regs l) -> Machine (M.insert oReg (op oVal oConst) regs) (max l oVal)

largest :: State Machine Int
largest = gets (maximum . M.elems . _regs)

largestEver :: State Machine Int
largestEver = gets _largest

run :: [Clause] -> State Machine ()
run = mapM_ interpret

instance Challenge [Clause] where
  parse = parseL pClause
  partOne cls = show $ evalState (run cls >> largest) (Machine M.empty 0)
  partTwo cls = show $ evalState (run cls >> largestEver) (Machine M.empty 0)

