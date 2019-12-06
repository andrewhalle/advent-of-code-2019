import System.Environment (getArgs)
import Data.List.Split

data VmState = Running  [Int] Int
             | Finished [Int]
             | Error    [Int]
             deriving (Show)

data ParamMode = Position
               | Immediate
               deriving (Show)

data Instruction = Add Int Int Int
            | Mult Int Int Int
            | Input Int
            | Output Int
            | JmpIfTrue Int Int
            | JmpIfFalse Int Int
            | LessThan Int Int Int
            | Equals Int Int Int
            | Halt
            deriving (Show)


replaceNth :: [Int] -> Int -> Int -> [Int]
replaceNth orig idx new =
  let (pre, _:suf) = splitAt idx orig
  in pre ++ new:suf

-- Vm possibly needs input to proceed, possibly outputs something.
-- up to vmMain to handle the actual IO
next :: (VmState, Maybe Int) -> (VmState, Maybe Int)
next (state@(Finished _), _) = (state, Nothing)
next (state@(Error _), _) = (state, Nothing)
next (state@(Running mem instrPtr), input) = apply instr state input
  where instr = nextInstruction mem instrPtr

-- Return the next instruction given memory and the instruction pointer
nextInstruction :: [Int] -> Int -> Instruction
nextInstruction mem instrPtr =
  let (opcode, p1mode, p2mode, p3mode) = parseInstruction (mem !! instrPtr)
  in case opcode of
    1  -> let (_:i1:i2:i3:_) = take 4 (drop instrPtr mem)
          in Add (p p1mode i1) (p p2mode i2) i3
    2  -> let (_:i1:i2:i3:_) = take 4 (drop instrPtr mem)
          in Mult (p p1mode i1) (p p2mode i2) i3
    3  -> let (_:i1:_) = take 2 (drop instrPtr mem)
          in Input i1
    4  -> let (_:i1:_) = take 2 (drop instrPtr mem)
          in Output i1
    5  -> let (_:i1:i2:_) = take 3 (drop instrPtr mem)
          in JmpIfTrue (p p1mode i1) (p p2mode i2)
    6  -> let (_:i1:i2:_) = take 3 (drop instrPtr mem)
          in JmpIfFalse (p p1mode i1) (p p2mode i2)
    7  -> let (_:i1:i2:i3:_) = take 4 (drop instrPtr mem)
          in LessThan (p p1mode i1) (p p2mode i2) i3
    8  -> let (_:i1:i2:i3:_) = take 4 (drop instrPtr mem)
          in Equals (p p1mode i1) (p p2mode i2) i3
    99 -> Halt
    n  -> error ("invalid opcode" ++ (show mem) ++ (show instrPtr))
  where p mode i = case mode of
                     Position -> mem !! i
                     Immediate -> i

-- get info from first int of instruction
parseInstruction :: Int -> (Int, ParamMode, ParamMode, ParamMode)
parseInstruction i = (i `mod` 100, p1mode, p2mode, p3mode)
  where p1mode = parseMode 100
        p2mode = parseMode 1000
        p3mode = parseMode 10000
        parseMode n = if getDigit n == 1 then Immediate else Position
        getDigit n = ((i `div` n) `mod` 10)

apply :: Instruction -> VmState -> Maybe Int -> (VmState, Maybe Int)
apply _ start@(Finished _) _ = (start, Nothing)
apply _ start@(Error _) _ = (start, Nothing)
apply instr (Running mem instrPtr) input =
  case instr of
    Add x1 x2 addr -> ((Running (replaceNth mem addr (x1 + x2)) (instrPtr + 4)), Nothing)
    Mult x1 x2 addr -> ((Running (replaceNth mem addr (x1 * x2)) (instrPtr + 4)), Nothing)
    Input addr -> case input of
                    Nothing -> error "no input"
                    Just n  -> ((Running (replaceNth mem addr n) (instrPtr + 2)), Nothing)
    Output addr -> ((Running mem (instrPtr + 2)), Just (mem !! addr))
    JmpIfTrue cond addr -> if cond /= 0
                           then ((Running mem addr), Nothing)
                           else ((Running mem (instrPtr+3)), Nothing)
    JmpIfFalse cond addr -> if cond == 0
                            then ((Running mem addr), Nothing)
                            else ((Running mem (instrPtr+3)), Nothing)
    LessThan x1 x2 addr ->  if x1 < x2
                            then ((Running (replaceNth mem addr 1) (instrPtr + 4)), Nothing)
                            else ((Running (replaceNth mem addr 0) (instrPtr + 4)), Nothing)
    Equals x1 x2 addr ->  if x1 == x2
                            then ((Running (replaceNth mem addr 1) (instrPtr + 4)), Nothing)
                            else ((Running (replaceNth mem addr 0) (instrPtr + 4)), Nothing)
    Halt -> ((Finished mem), Nothing)

ioNext :: VmState -> IO (VmState, Maybe Int)
ioNext curr@(Error _) = return (curr, Nothing)
ioNext curr@(Finished _) = return (curr, Nothing)
ioNext curr@(Running mem instrPtr) =
  let n = nextInstruction mem instrPtr
  in case n of
    Input _ -> do
                  strNum <- getLine
                  let num = (read strNum) :: Int
                  return (next (curr, Just num))
    _       -> do
                  let (newState, output) = next (curr, Nothing)
                  case output of
                    Nothing -> return (newState, Nothing)
                    Just n  -> return (newState, Just n)

vmLoop :: VmState -> IO ()
vmLoop curr =
  case curr of
    Error _ -> print curr
    Finished _ -> print curr
    Running _ _ -> do (next, out) <- ioNext curr
                      case out of
                        Nothing -> return ()
                        Just n -> print n
                      vmLoop next

vmMain :: String -> IO ()
vmMain input =
  let init = map (read :: String -> Int) (splitOn "," input)
      in vmLoop (Running init 0)

-- need main to parse program into list of ints, then run vmMain which looks ahead to the next
-- instruction, and if it requires input, does IO, and if it returns output, prints it

interactWith function inputFile outputFile = do
  input <- readFile inputFile
  writeFile outputFile (function input)

main = mainWith myFunction
  where mainWith function = do
          args <- getArgs
          case args of
            [input, output] -> do
              inContents <- readFile input
              vmMain inContents
            _ -> putStrLn "error: exactly two arguments needed"

        myFunction = vmMain
