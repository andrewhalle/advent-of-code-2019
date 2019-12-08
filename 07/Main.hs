module Main where

import Computer
import System.Environment
import Data.List

replaceVm :: [VmState] -> Int -> VmState -> [VmState]
replaceVm vms idx new =
   let (pre,_:suf) = splitAt idx vms
   in pre ++ (new:suf)

-- takes a list of vm states, input to pass, and the index of the vm to pass
-- it to
complete :: ([VmState], [Int], Int) -> Int
complete (vms, input, curr) =
  let currentState        = vms !! curr
      (nextState, output) = vmRunUntilMoreInputRequired currentState input
  in if (curr == ((length vms) - 1)) && (isFinished nextState)
     then head output
     else complete ((replaceVm vms curr nextState), output, nextVm)
  where isFinished curr =
          case curr of
            Finished _ -> True
            _          -> False
        nextVm = (curr + 1) `mod` (length vms)

largestPossibleOutput :: String -> Int
largestPossibleOutput prog =
  let phases = permutations [5..9]
  in maximum (map getOutput phases)
  where initState = vmInitState prog
        constructInitial p' = let (state, []) = vmRunUntilMoreInputRequired initState [p']
                              in state
        getOutput p =
          let init = (map constructInitial p, [0], 0)
          in complete init


main :: IO ()
main = do
  [progFilename] <- getArgs
  prog <- readFile progFilename
  print $ largestPossibleOutput prog
