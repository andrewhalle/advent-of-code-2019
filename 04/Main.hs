import System.Environment (getArgs)
import Data.List
import qualified Data.Set as Set

hasRepeatedDigit :: String -> Bool
hasRepeatedDigit (x1:x2:[]) = x1 == x2
hasRepeatedDigit (x1:x2:xs) = x1 == x2 || (hasRepeatedDigit (x2:xs))

splitContinuous :: [Char] -> [[Char]]
splitContinuous [] = []
splitContinuous (x:[]) = [[x]]
splitContinuous (x:xs) = (x:(takeWhile (==x) xs)):(splitContinuous (dropWhile (==x) xs))

repeatsExact :: String -> Bool
repeatsExact n = (length (filter (==2) (map length (splitContinuous n)))) /= 0

ascendingDigits :: String -> Bool
ascendingDigits n = (sort n) == n
  
countPasswords :: Int -> Int -> Int
countPasswords min max =
  length (filter valid [min..max])
  where valid n = (repeatsExact (show n)) && (ascendingDigits (show n))

interactWith function inputFile outputFile = do
  input <- readFile inputFile
  writeFile outputFile (function input)

main = mainWith myFunction
  where mainWith function = do
          args <- getArgs
          case args of
            [input, output] -> interactWith function input output
            _ -> putStrLn "error: exactly two arguments needed"

        myFunction = (\_ -> show (countPasswords 124075 580796))
