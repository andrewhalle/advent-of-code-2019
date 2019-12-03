import System.Environment (getArgs)
import Data.List.Split

data VmState = Running  [Int] Int
             | Finished [Int]
             | Error    [Int]
             deriving (Show)

replaceNth :: [Int] -> Int -> Int -> [Int]
replaceNth orig idx new =
  let (pre, _:suf) = splitAt idx orig
  in pre ++ new:suf

next :: VmState -> VmState
next (Running nums instr_ptr)
  | nums !! instr_ptr ==  1 = apply (+)
  | nums !! instr_ptr ==  2 = apply (*)
  | nums !! instr_ptr == 99 = (Finished nums)
  | otherwise               = (Error nums)
  where deref offset = (nums !! (instr_ptr + offset))
        derefTwice offset = nums !! (deref offset)
        nextInstrPtr = instr_ptr + 4
        apply op = (Running (replaceNth nums (deref 3) ((derefTwice 1) `op` (derefTwice 2))) nextInstrPtr)
next state = state

run :: VmState -> VmState
run state@(Running _ _) = run (next state)
run state = state

runToCompletion :: [Int] -> VmState
runToCompletion init =
  let initState = (Running init 0)
  in run initState

findOneTwoOutputEquals :: Int -> [Int] -> (Int, Int)
findOneTwoOutputEquals output templateMemory =
  fst (head (filter eqOutput [ (pair, f) | (pair, (Finished (f:_))) <- (zip allOneTwoPairs (map runToCompletion (map buildInit allOneTwoPairs)))]))
  where allOneTwoPairs = [ (a,b) | a <- [0..99], b <- [0..99]]
        pre = head templateMemory
        suf = tail templateMemory
        buildInit (a,b) = pre:a:b:suf
        eqOutput (_,c) = c == output

vmMain :: String -> String
vmMain input =
  show (findOneTwoOutputEquals 19690720 init)
    where init = map (read :: String -> Int) (splitOn "," input)

-- interact framework below

interactWith function inputFile outputFile = do
  input <- readFile inputFile
  writeFile outputFile (function input)

main = mainWith myFunction
  where mainWith function = do
          args <- getArgs
          case args of
            [input, output] -> interactWith function input output
            _ -> putStrLn "error: exactly two arguments needed"

        myFunction = vmMain
