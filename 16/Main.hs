module Main where

import Data.List

-- apply one phase of FFT, considering the offset
phase :: [Int] -> [Int]
phase input =
  let x = foldr (\x y -> (x + (fst y), ((abs (x + (fst y))) `mod` 10):(snd y))) (0, []) input
  in snd x

-- get list of digits from input string
getInput :: String -> [Int]
getInput s = map toDigit (filter (/='\n') s)
  where toDigit c = case c of
                      '1' -> 1
                      '2' -> 2
                      '3' -> 3
                      '4' -> 4
                      '5' -> 5
                      '6' -> 6
                      '7' -> 7
                      '8' -> 8
                      '9' -> 9
                      '0' -> 0


toChar d = case d of
             1 -> '1'
             2 -> '2'
             3 -> '3'
             4 -> '4'
             5 -> '5'
             6 -> '6'
             7 -> '7'
             8 -> '8'
             9 -> '9'
             0 -> '0'

main :: IO ()
main = do
  inputStr <- readFile "puzzle.txt"
  let input = getInput inputStr
      rep = cycle input
      offset = read (((map toChar) . (take 7)) input)
      input' = take ((length input) * 10000) rep
      input'' = drop offset input'
      result = last (take 101 (iterate phase input''))
  print $ take 8 result
