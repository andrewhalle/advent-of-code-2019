module Main where

import Computer (runVmWithInput)
import System.Environment
import Data.List

applyAllAmplifiers :: String -> [Int] -> Int
applyAllAmplifiers prog (p1:p2:p3:p4:p5:[]) =
  let (o1:[]) = runVmWithInput prog [p1, 0]
      (o2:[]) = runVmWithInput prog [p2, o1]
      (o3:[]) = runVmWithInput prog [p3, o2]
      (o4:[]) = runVmWithInput prog [p4, o3]
      (o5:[]) = runVmWithInput prog [p5, o4]
  in o5
  
main :: IO ()
main = do
  [progFilename] <- getArgs
  prog <- readFile progFilename
  print $ runVmWithInput prog [5,0]
