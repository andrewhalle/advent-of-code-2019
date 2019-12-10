module Main where

import Computer
import System.Environment

main :: IO ()
main = do
  [progFile] <- getArgs
  program <- readFile progFile
  let memsize = 2048
  let initState = vmInitState program memsize
      input = []
      (end, output) = vmRunUntilMoreInputRequired initState input
  print $ output
