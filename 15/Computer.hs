module Computer where

import Data.List.Split

data VmState = Running  [Int] Int Int
             | Finished [Int]
             | Error    [Int]
             deriving (Show)

data ParamMode = Position
               | Immediate
               | Relative
               deriving (Show)

data Instruction = Add Int Int Int
            | Mult Int Int Int
            | Input Int
            | Output Int
            | JmpIfTrue Int Int
            | JmpIfFalse Int Int
            | LessThan Int Int Int
            | Equals Int Int Int
            | AdjustRelativeOffset Int
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
next (state@(Running mem instrPtr relOff), input) = apply instr state input
  where instr = nextInstruction mem instrPtr relOff

-- Return the next instruction given memory and the instruction pointer
nextInstruction :: [Int] -> Int -> Int -> Instruction
nextInstruction mem instrPtr relOff =
  let (opcode, p1mode, p2mode, p3mode) = parseInstruction (mem !! instrPtr)
  in case opcode of
    1  -> let (_:i1:i2:i3:_) = take 4 (drop instrPtr mem)
          in Add (p p1mode i1) (p p2mode i2) (a p3mode i3)
    2  -> let (_:i1:i2:i3:_) = take 4 (drop instrPtr mem)
          in Mult (p p1mode i1) (p p2mode i2) (a p3mode i3)
    3  -> let (_:i1:_) = take 2 (drop instrPtr mem)
          in Input (a p1mode i1)
    4  -> let (_:i1:_) = take 2 (drop instrPtr mem)
          in Output (p p1mode i1)
    5  -> let (_:i1:i2:_) = take 3 (drop instrPtr mem)
          in JmpIfTrue (p p1mode i1) (p p2mode i2)
    6  -> let (_:i1:i2:_) = take 3 (drop instrPtr mem)
          in JmpIfFalse (p p1mode i1) (p p2mode i2)
    7  -> let (_:i1:i2:i3:_) = take 4 (drop instrPtr mem)
          in LessThan (p p1mode i1) (p p2mode i2) (a p3mode i3)
    8  -> let (_:i1:i2:i3:_) = take 4 (drop instrPtr mem)
          in Equals (p p1mode i1) (p p2mode i2) (a p3mode i3)
    9  -> let (_:i1:_) = take 2 (drop instrPtr mem)
          in AdjustRelativeOffset (p p1mode i1)
    99 -> Halt
    n  -> error ("invalid opcode" ++ (show mem) ++ (show instrPtr))
  where p mode i = case mode of
                     Position -> mem !! i
                     Immediate -> i
                     Relative -> mem !! (i + relOff)
        a mode i = case mode of
                     Position -> i
                     Immediate -> error "can't treat immediate value as address"
                     Relative -> i + relOff

-- get info from first int of instruction
parseInstruction :: Int -> (Int, ParamMode, ParamMode, ParamMode)
parseInstruction i = (i `mod` 100, p1mode, p2mode, p3mode)
  where p1mode = parseMode 100
        p2mode = parseMode 1000
        p3mode = parseMode 10000
        parseMode n = case (getDigit n) of
                        0 -> Position
                        1 -> Immediate
                        2 -> Relative
                        _ -> error "invalid parameter mode"
        getDigit n = ((i `div` n) `mod` 10)

apply :: Instruction -> VmState -> Maybe Int -> (VmState, Maybe Int)
apply _ start@(Finished _) _ = (start, Nothing)
apply _ start@(Error _) _ = (start, Nothing)
apply instr (Running mem instrPtr relOff) input =
  case instr of
    Add x1 x2 addr -> ((Running (replaceNth mem addr (x1 + x2)) (instrPtr + 4) relOff), Nothing)
    Mult x1 x2 addr -> ((Running (replaceNth mem addr (x1 * x2)) (instrPtr + 4) relOff), Nothing)
    Input addr -> case input of
                    Nothing -> error "no input"
                    Just n  -> ((Running (replaceNth mem addr n) (instrPtr + 2) relOff), Nothing)
    Output val -> ((Running mem (instrPtr + 2) relOff), Just val)
    JmpIfTrue cond addr -> if cond /= 0
                           then ((Running mem addr relOff), Nothing)
                           else ((Running mem (instrPtr+3) relOff), Nothing)
    JmpIfFalse cond addr -> if cond == 0
                            then ((Running mem addr relOff), Nothing)
                            else ((Running mem (instrPtr+3) relOff), Nothing)
    LessThan x1 x2 addr ->  if x1 < x2
                            then ((Running (replaceNth mem addr 1) (instrPtr + 4) relOff), Nothing)
                            else ((Running (replaceNth mem addr 0) (instrPtr + 4) relOff), Nothing)
    Equals x1 x2 addr ->  if x1 == x2
                            then ((Running (replaceNth mem addr 1) (instrPtr + 4) relOff), Nothing)
                            else ((Running (replaceNth mem addr 0) (instrPtr + 4) relOff), Nothing)
    AdjustRelativeOffset v -> ((Running mem (instrPtr + 2) (relOff + v)), Nothing)
    Halt -> ((Finished mem), Nothing)

vmLoop :: VmState -> [Int] -> [Int]
vmLoop curr input =
  case curr of
    Error _                     -> []
    Finished _                  -> []
    Running mem instrPtr relOff -> let n = nextInstruction mem instrPtr relOff
                                       (nextState, out) = case n of
                                                            Input _ -> next (curr, Just (head input))
                                                            _       -> next (curr, Nothing)
                                       nextInput = case n of
                                                     Input _ -> tail input
                                                     _       -> input
                                   in case out of
                                        Just n -> (n:(vmLoop nextState nextInput))
                                        _      -> vmLoop nextState nextInput

-- given a VmState and a list of input, run until either
--   a. the vm finishes
--   b. the vm requests more input than in given
-- in either case, return the current VM state, and any output produced
-- until stopping condition
vmRunUntilMoreInputRequired :: VmState -> [Int] -> (VmState, [Int])
vmRunUntilMoreInputRequired curr input =
  case curr of
    Error _                     -> (curr, [])
    Finished _                  -> (curr, [])
    Running mem instrPtr relOff -> let n = nextInstruction mem instrPtr relOff
                            in case n of
                                 Input _ -> if null input
                                            then (curr, [])
                                            else let (n, Nothing) = next (curr, Just (head input))
                                                 in vmRunUntilMoreInputRequired n (tail input)
                                 _       -> let (n, out) = next (curr, Nothing)
                                            in case out of
                                                 Nothing -> vmRunUntilMoreInputRequired n input
                                                 Just n' -> let (s, o') = vmRunUntilMoreInputRequired n input
                                                            in (s, n':o')

runVmWithInput :: String -> [Int] -> [Int]
runVmWithInput prog input =
  let initMem = map (read :: String -> Int) (splitOn "," prog)
  in vmLoop (Running initMem 0 0) input

vmInitState :: String -> Int -> VmState
vmInitState prog memsize =
  let progMem = map (read :: String -> Int) (splitOn "," prog)
      blank   = replicate (memsize - (length progMem)) 0
      initMem = progMem ++ blank
  in (Running initMem 0 0)
