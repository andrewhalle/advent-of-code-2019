module Main where

-- generates an infinite list with the pattern according to
-- the position of the digit we're calculating
pattern :: Int -> [Int]
pattern idx =
  let n = idx + 1
      pat = (take n (repeat 0)) ++ (take n (repeat 1)) ++ (take n (repeat 0)) ++ (take n (repeat (-1)))
      rep = cycle pat
  in tail rep

-- calculate one output digit at position
calcDigit :: [Int] -> Int -> Int
calcDigit input idx =
  let pat = pattern idx
      zipped = zip input pat
      total = foldr (\x y -> y + ((fst x) * (snd x))) 0 zipped
  in (abs total) `mod` 10

-- apply one phase of FFT
phase :: [Int] -> [Int]
phase input = take (length input) (map (calcDigit input) [0..])

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

main :: IO ()
main = do
  inputStr <- readFile "puzzle.txt"
  let input = getInput inputStr
      result = last (take 101 (iterate phase input))
  print $ take 8 result
