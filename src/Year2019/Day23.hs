module Year2019.Day23 where

import Challenge
import qualified Intcode as IC

import Prelude hiding (init)

import qualified Data.Map.Strict as M
import           Control.Monad.State.Lazy
import Debug.Trace

data Packet = Packet { _x :: Int, _y :: Int } deriving Show
data Server = Server { _ip :: Int, _process :: IC.ProcessState }
type Mailbox = M.Map Int [Packet]

type NetWorkState a = State (Mailbox, Maybe Packet, Maybe Packet) a

getMessage :: [Int] -> [(Int, Packet)]
getMessage [] = []
getMessage (r:x:y:rest) = (r, Packet x y):getMessage rest
getMessage _ = []

init :: IC.Program -> [Server]
init program = do
  n <- [0..49]
  pure . Server n $ IC.initializeProcess program [n]

putInMailBox :: Int -> Packet -> NetWorkState ()
putInMailBox d p = modify' $ \(m, n, np) -> (M.insertWith (flip (++)) d [p] m, n, np)

checkForNatMessage :: NetWorkState (Maybe Packet)
checkForNatMessage = do
  (mailbox, nat, _) <- get
  case M.lookup 255 mailbox of
    Nothing -> return Nothing
    Just ps -> do
      put (M.delete 255 mailbox, Just $ last ps, nat)
      return (Just $ last ps)

getMessages :: [[Int]] -> NetWorkState [Bool]
getMessages outputs = forM outputs $ \o -> do
  let packets = getMessage o
  forM_ packets $ uncurry putInMailBox
  return (not $ null packets)

popMessage :: Int -> NetWorkState [Int]
popMessage n = do
  (mailbox, _, _) <- get
  case M.lookup n mailbox of
    Nothing -> return [-1]
    Just [] -> return [-1]
    Just (Packet x y:rest) -> do
      modify' $ \(m, na, np) -> (M.insert n rest m, na, np)
      return [x, y]

loadMessages :: [Server] -> NetWorkState [Server]
loadMessages processes = do
  forM processes $ \(Server n process) -> do
    values <- popMessage n
    let nextProcess = IC.runMachine process $ do IC.addProcessInputs values
    return $ Server n nextProcess

runServers :: [Server] -> [([Int], Server)]
runServers servers = do
  (Server n process) <- servers
  (outs, nextProcess) <- pure $ runState IC.continueExecution process
  pure (outs, Server n nextProcess)

stepServers :: [Server] -> NetWorkState [Server]
stepServers processes = do
  let res = runServers processes
  _ <- getMessages (map fst res)
  loadMessages (map snd res)

runNetwork :: [Server] -> Int
runNetwork = flip evalState (M.empty, Nothing, Nothing) . stepPt1

stepPt1 :: [Server] -> NetWorkState Int
stepPt1 processes = do
   res <- checkForNatMessage
   loadedPs <- stepServers processes
   case res of
     Nothing -> stepPt1 loadedPs
     Just (Packet _ y) -> return y

waiting :: [Int] -> Bool
waiting ((-1):(-1):_) = True
waiting (_:rest) = waiting rest
waiting _ = False

networkIdle :: [[Int]] -> [IC.ProcessState] -> NetWorkState Bool
networkIdle outs processes = do
  (mailbox, _, _) <- get
  let
    allOutputEmpty = all null outs
    allBoxesEmpty = all null (M.elems mailbox)
    allWaiting = all waiting $ forM processes $ \p -> IC.withMachine p IC.peekProcessInputs
  return $ allBoxesEmpty && allWaiting && allOutputEmpty

fireNat :: NetWorkState (Maybe Int)
fireNat = do
  (m, nat, prevNat) <- get
  case nat of
    Nothing -> return Nothing
    Just packet -> do
      put (M.insert 0 [packet] m, nat, prevNat)
      traceShowM nat
      traceShowM prevNat
      case prevNat of
        Nothing -> return Nothing
        Just p -> if _y packet == _y p then return (Just $ _y p) else return Nothing

stepPt2 :: [Server] -> NetWorkState Int
stepPt2 processes = do
  _ <- checkForNatMessage
  let res = runServers processes
  outs <- getMessages (map fst res)
  loadedPs <- loadMessages (map snd res)
  idle <- networkIdle (map fst res) (map (_process . snd) res)
  val <- if idle
      then fireNat
      else return Nothing
  case val of
    Nothing -> stepPt2 loadedPs
    Just va -> return va

-- | TODO: answer is 14370 by educated guess
-- | this day needs to be rewritten bad
-- | maybe along with the machine
instance Challenge IC.Program where
  parse = IC.fromString
  partOne = show . runNetwork . init
  partTwo = show . flip evalState (M.empty, Nothing, Nothing) . stepPt2 . init