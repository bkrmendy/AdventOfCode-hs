module Intcode (
  Program(Program, unProgram)
  , fromString
  , continueExecution
  , executeCode
  , readProcessMemory
  , initializeProcess
  , runCode
) where
import Data.List.Split (splitOn)
import qualified Data.HashMap.Strict as Map
import Data.Digits
import Control.Monad.State
import Debug.Trace
newtype Program = Program { unProgram :: [Int] }

fromString :: String -> Program
fromString = Program . map read . splitOn ","

newtype Memory = Memory { unMemory :: Map.HashMap Int Int } deriving (Eq, Show)

readMemory :: Int -> State Memory Int
readMemory pointer = gets $ \(Memory m) -> Map.lookupDefault 0 pointer m

writeToMemory :: Int -> Int -> State Memory ()
writeToMemory pointer 0 = modify $ \(Memory m) -> Memory $ Map.delete pointer m
writeToMemory pointer value = modify $ \(Memory m) -> Memory $  Map.alter (\_ -> Just value) pointer m

fromProgram :: Program -> Memory
fromProgram = Memory . Map.fromList . zip [0..] . unProgram

newtype InputStream = InputStream { unInputStream :: [Int] } deriving (Eq, Show)

addInput :: Int -> State InputStream ()
addInput input = modify $ \(InputStream is) -> InputStream $ is ++ [input]

addInputs :: [Int] -> State InputStream ()
addInputs inputs = modify $ \(InputStream is) -> InputStream $ is ++ inputs

popInput :: State InputStream (Maybe Int)
popInput = state $ \input@(InputStream xs) -> case xs of
    [] -> (Nothing, input)
    first:rest -> (Just first, InputStream rest)

data ExecutionState = Running | Blocked | Terminated | Error deriving (Eq, Show, Enum)

data ProcessState = ProcessState { _memory :: Memory
                                 , _inputs :: InputStream
                                 , _pc :: Int
                                 , _relativeBase :: Int
                                 , _status :: ExecutionState
                                 }

processStatus :: State ProcessState ExecutionState
processStatus = gets _status

hasShutDown :: State ProcessState Bool
hasShutDown = do
  thisStatus <- processStatus
  case thisStatus of
    Terminated  -> return True
    Error       -> return True
    _           -> return False

isRunning :: State ProcessState Bool
isRunning = do
  thisStatus <- processStatus
  case thisStatus of
    Running -> return True
    _       -> return False

setProcessStatus :: ExecutionState -> State ProcessState ()
setProcessStatus newStatus = do
  stopped <- hasShutDown
  if stopped
    then return ()
    else modify $ \ps -> ps { _status = newStatus }

terminateProcess :: State ProcessState ()
terminateProcess = setProcessStatus Terminated

abortProcess :: State ProcessState ()
abortProcess = setProcessStatus Error

setPC :: Int -> State ProcessState ()
setPC newPC = modify $ \ps -> ps { _pc = newPC }

processPC :: State ProcessState Int
processPC = gets _pc

incrementPC :: Int -> State ProcessState ()
incrementPC offset = do
  thisPC <- processPC
  setPC (thisPC + offset)

setRelativeBasePointer :: Int -> State ProcessState ()
setRelativeBasePointer pointer = modify $ \s -> s { _relativeBase = pointer }

processBasePointer :: State ProcessState Int
processBasePointer = gets _relativeBase

incrementBasePointer :: Int -> State ProcessState ()
incrementBasePointer offset = do
  thisBase <- processBasePointer
  setRelativeBasePointer (thisBase + offset)

readProcessMemory :: Int -> State ProcessState Int
readProcessMemory pointer = gets $ \ProcessState { _memory = m } -> evalState (readMemory pointer) m

writeToProcessMemory :: Int -> Int -> State ProcessState ()
writeToProcessMemory pointer value =
  modify $ \s@ProcessState{_memory = m} -> s {_memory = execState (writeToMemory pointer value) m}

addProcessInput :: Int -> State ProcessState ()
addProcessInput additionalInput =
  modify $ \s@ProcessState{ _inputs = inputStream } -> s {_inputs = execState (addInput additionalInput) inputStream}

addProcessInputs :: [Int] -> State ProcessState ()
addProcessInputs additionalInputs =
  modify $ \s@ProcessState{ _inputs = inputStream } -> s {_inputs = execState (addInputs additionalInputs) inputStream}

popProcessInput :: State ProcessState (Maybe Int)
popProcessInput = state $ \s@ProcessState{ _inputs = inputStream } ->
    let (input, newInputs) = runState popInput inputStream
        in (input, s { _inputs = newInputs })

initializeProcess :: Program -> [Int] -> ProcessState
initializeProcess code initialInputs = ProcessState {
      _memory = fromProgram code,
      _inputs = InputStream initialInputs,
      _pc = 0,
      _relativeBase = 0,
      _status = Running
}

type Arguments = [Int]

data Opcode = Add | Multiply | Get | Put | JumpIfTrue | JumpIfFalse | LessThan | Equals | IncrementRelativeBase | Stop deriving (Eq, Show, Enum)
toOpcode :: Int -> Maybe Opcode
toOpcode 1 = Just Add
toOpcode 2 = Just Multiply
toOpcode 3 = Just Get
toOpcode 4 = Just Put
toOpcode 5 = Just JumpIfTrue
toOpcode 6 = Just JumpIfFalse
toOpcode 7 = Just LessThan
toOpcode 8 = Just Equals
toOpcode 9 = Just IncrementRelativeBase
toOpcode 99 = Just Stop
toOpcode _ = Nothing

data OperationMode = Read | Write deriving (Eq, Show)

associatedOperationModes :: Opcode -> [OperationMode]
associatedOperationModes Add = [Read, Read, Write]
associatedOperationModes Multiply = [Read, Read, Write]
associatedOperationModes Get = [Write]
associatedOperationModes Put = [Read]
associatedOperationModes JumpIfTrue = [Read, Read]
associatedOperationModes JumpIfFalse = [Read, Read]
associatedOperationModes LessThan = [Read, Read, Write]
associatedOperationModes Equals = [Read, Read, Write]
associatedOperationModes IncrementRelativeBase = [Read]
associatedOperationModes Stop = []

associatedOperation :: Opcode -> (Arguments -> State ProcessState (Maybe Int))
associatedOperation Add = handleTerminationAndRun . add
associatedOperation Multiply = handleTerminationAndRun . mul
associatedOperation Get = handleTerminationAndRun . getOperation
associatedOperation Put = handleTerminationAndRun . putOperation
associatedOperation JumpIfTrue = handleTerminationAndRun . jumpIfTrue
associatedOperation JumpIfFalse = handleTerminationAndRun . jumpIfFalse
associatedOperation LessThan = handleTerminationAndRun . lessThan
associatedOperation Equals = handleTerminationAndRun . equals
associatedOperation IncrementRelativeBase = handleTerminationAndRun . incrementRelativeBase
associatedOperation Stop = handleTerminationAndRun . stop

data ArgumentMode = Pointer | Value | Relative deriving (Eq, Show)

toArgumentMode :: Int -> Maybe ArgumentMode
toArgumentMode 0 = Just Pointer
toArgumentMode 1 = Just Value
toArgumentMode 2 = Just Relative
toArgumentMode _ = Nothing

data ArgumentSpecification = ArgumentSpecification {
    argumentMode :: ArgumentMode,
    operationMode :: OperationMode
} deriving (Eq, Show)

data Instruction = Instruction { _opcode :: Opcode
                               , _argumentSpecifications :: [ArgumentSpecification]
                               }

instructionArgument :: Int -> (Int, ArgumentSpecification) -> State ProcessState Int
instructionArgument basePtr (offset, argSpec) =
  let evalPtr = basePtr + offset
    in case argumentMode argSpec of
      Value -> readProcessMemory evalPtr
      Pointer -> case operationMode argSpec of
                    Write -> readProcessMemory evalPtr
                    Read -> do
                      transitiveEvaluationPointer <- readProcessMemory evalPtr
                      readProcessMemory transitiveEvaluationPointer
      Relative -> do
        relativeBase <- processBasePointer
        baseIncr <- readProcessMemory evalPtr
        let targetPtr = relativeBase + baseIncr
          in case operationMode argSpec of
            Write -> return targetPtr
            Read -> readProcessMemory targetPtr

instructionArguments :: Instruction -> State ProcessState Arguments
instructionArguments instr = do
  base <- processBasePointer
  let enumeratedArgs = zip [1..] (_argumentSpecifications instr)
    in mapM (instructionArgument base) enumeratedArgs

argumentModesFromSpecifier :: Int -> Maybe [Maybe ArgumentMode]
argumentModesFromSpecifier 0 = Just []
argumentModesFromSpecifier x
    | x < 0 = Nothing
    | otherwise = Just (map toArgumentMode (reverse (digits 10 x)))

toArgumentSpecifications :: Opcode -> Int -> Maybe [ArgumentSpecification]
toArgumentSpecifications code argspec = do
  maybeArgModes <- argumentModesFromSpecifier argspec
  specifiedArgumentModes <- sequence maybeArgModes
  let operationModes = associatedOperationModes code
      nMissingElements = length operationModes - length specifiedArgumentModes
      in if nMissingElements < 0
            then Nothing
            else let paddedArgumentsModes = specifiedArgumentModes ++ replicate nMissingElements Pointer
                  in return (zipWith ArgumentSpecification paddedArgumentsModes operationModes)

intcodeInstruction :: State ProcessState (Maybe Instruction)
intcodeInstruction = do
  instructionPtr <- processPC
  instructionValue <- readProcessMemory instructionPtr
  return (do
    _opcode <- toOpcode (instructionValue `mod` 100)
    argumentSpecs <- toArgumentSpecifications _opcode (instructionValue `div` 100)
    return (Instruction _opcode argumentSpecs))

handleTerminationAndRun :: State ProcessState (Maybe Int) -> State ProcessState (Maybe Int)
handleTerminationAndRun state = do
  stopped <- hasShutDown
  if stopped
    then return Nothing
    else state

applyBinaryOperationAndWrite :: (Int -> Int -> Int) -> (Arguments -> State ProcessState (Maybe Int))
applyBinaryOperationAndWrite binaryOp arguments = do
    let
        targetPointer = arguments !! 2
        value = binaryOp (head arguments) (arguments !! 1)
        in writeToProcessMemory targetPointer value
    incrementPC 4
    setProcessStatus Running
    return Nothing

add :: Arguments -> State ProcessState (Maybe Int)
add = applyBinaryOperationAndWrite (+)

mul :: Arguments -> State ProcessState (Maybe Int)
mul = applyBinaryOperationAndWrite (*)

applyBinaryComparisonAndWrite :: (Int -> Int -> Bool) -> (Arguments -> State ProcessState (Maybe Int))
applyBinaryComparisonAndWrite binaryComp arguments = do
    let
        targetPointer = arguments !! 2
        value = if (head arguments) `binaryComp` (arguments!!1)
            then 1
            else 0
        in writeToProcessMemory targetPointer value
    incrementPC 4
    setProcessStatus Running
    return Nothing

lessThan :: Arguments -> State ProcessState (Maybe Int)
lessThan = applyBinaryComparisonAndWrite (<)

equals :: Arguments -> State ProcessState (Maybe Int)
equals = applyBinaryComparisonAndWrite (==)

jumpIfTrue :: Arguments -> State ProcessState (Maybe Int)
jumpIfTrue = jumpIf (/= 0)

jumpIfFalse :: Arguments -> State ProcessState (Maybe Int)
jumpIfFalse = jumpIf (== 0)

jumpIf :: (Int -> Bool) -> (Arguments -> State ProcessState (Maybe Int))
jumpIf test arguments = do
    if test (head arguments)
        then setPC (arguments !! 1)
        else incrementPC 3
    setProcessStatus Running
    return Nothing

incrementRelativeBase :: Arguments -> State ProcessState (Maybe Int)
incrementRelativeBase arguments = do
    incrementBasePointer $ head arguments
    incrementPC 2
    setProcessStatus Running
    return Nothing

getOperation :: Arguments -> State ProcessState (Maybe Int)
getOperation arguments = do
    maybeInput <- popProcessInput
    case maybeInput of
        Nothing -> do
            setProcessStatus Blocked
            return Nothing
        Just input -> do
            let
                targetPointer = head arguments
                in writeToProcessMemory targetPointer input
            incrementPC 2
            setProcessStatus Running
            return Nothing

putOperation :: Arguments -> State ProcessState (Maybe Int)
putOperation arguments = do
    incrementPC 2
    setProcessStatus Running
    let newOutputValue = head arguments
        in return $ Just newOutputValue

stop :: Arguments -> State ProcessState (Maybe Int)
stop _ = do
    terminateProcess
    return Nothing

continueExecution :: State ProcessState [Int]
continueExecution = do
    maybeResult <- executeNextInstruction
    running <- isRunning
    if running
        then do
            remainingResult <- continueExecution
            case maybeResult of
                Nothing -> return remainingResult
                Just result -> return (result:remainingResult)
        else return []

executeNextInstruction :: State ProcessState (Maybe Int)
executeNextInstruction = do
    maybeInstruction <- intcodeInstruction
    case maybeInstruction of
        Nothing -> do
            abortProcess
            return Nothing
        Just instruction -> executeInstruction instruction

executeInstruction :: Instruction -> State ProcessState (Maybe Int)
executeInstruction instruction = do
    arguments <- instructionArguments instruction
    let operation = associatedOperation (_opcode instruction)
        in operation arguments

runUntilTerminated :: State ProcessState ()
runUntilTerminated = do
  running <- isRunning
  when running $ do
      _ <- executeNextInstruction
      i <- readProcessMemory 0
      traceM (show i)
      runUntilTerminated

runCode :: Program -> Int
runCode code =
  let initialState = initializeProcess code []
  in evalState (do
    runUntilTerminated
    readProcessMemory 0
  ) initialState

executeCode :: Program -> [Int] -> [Int]
executeCode code initialInputs =
    let initialState = initializeProcess code initialInputs
        in evalState continueExecution initialState