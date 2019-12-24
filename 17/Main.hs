module Main where

import Computer
import Data.Char
import Data.List
import qualified Data.Set as S
import Data.List.Split

-- according to the map, is this index the intersection of
-- two scaffolding?
isIntersection :: [Int] -> Int -> Int -> Int -> Bool
isIntersection xs width height idx
  | idx < width = False
  | (idx `div` width) >= (height-1) = False
  | (idx `mod` width) == 0 = False
  | (idx `mod` width) == (width - 1) = False
  | otherwise = let curr = xs !! idx
                    top = xs !! (idx - width)
                    right = xs !! (idx + 1)
                    down = xs !! (idx + width)
                    left = xs !! (idx)
                in and (map (==(ord '#')) [curr, top, right, down, left])

tokenize :: String -> [String]
tokenize s = splitOn "," s

sublist :: [String] -> Int -> Int -> [String]
sublist ss start end = take (end-start) (drop start ss)

allSublists :: [String] -> [([String], (Int,Int))]
allSublists ss = [ (sublist ss x y, (x,y)) | x <- [0..(length ss)], y <- [(x+1)..(length ss)] ]

allSublistsWithoutRep :: [String] -> [([String], (Int,Int))]
allSublistsWithoutRep ss =
  let sub = allSublists ss
  in filter (\x -> ("A" `notElem` (fst x)) && ("B" `notElem` (fst x)) && ("C" `notElem` (fst x))) sub

replaceSublist :: [String] -> Int -> Int -> [String]
replaceSublist ss start end = (take start ss) ++ (("#"):(drop end ss))

isRepeated :: [String] -> ([String], (Int,Int)) -> Bool
isRepeated base (pat, (x,y)) =
  let rep = replaceSublist base x y
  in pat `isInfixOf` rep

allRepeatedSublists :: [String] -> [[String]]
allRepeatedSublists ss =
  let sub = allSublistsWithoutRep ss
      rep = filter (isRepeated ss) sub
  in S.toList (S.fromList (map fst rep))

fitsInMemory :: [String] -> Bool
fitsInMemory ss =
  let ls = map length ss
  in ((sum ls) + (length ss) - 1) <= 20

possibleSubstitutions :: [String] -> [[String]]
possibleSubstitutions ss = filter fitsInMemory (allRepeatedSublists ss)

replaceAll :: [String] -> [String] -> String -> [String]
replaceAll base pat rep
  | (length base) < (length pat) = base
  | pat `isPrefixOf` base = rep:(replaceAll (drop (length pat) base) pat rep)
  | otherwise = (head base):(replaceAll (tail base) pat rep)


data State = State { program :: [String], aPat :: Maybe [String], bPat :: Maybe [String], cPat :: Maybe [String] }
             deriving (Show)

next :: State -> [State]
next curr@(State { program=program, aPat=a, bPat=b, cPat=c }) =
  let ps = possibleSubstitutions program
  in map buildState ps
  where buildState sub =
          let subLabel = if (missing a)
                         then "A"
                         else if (missing b)
                              then "B"
                              else "C"
              newProgram = replaceAll program sub subLabel
          in curr {
               program=newProgram,
               aPat=(if subLabel == "A" then Just sub else a),
               bPat=(if subLabel == "B" then Just sub else b),
               cPat=(if subLabel == "C" then Just sub else c) }
        missing x = case x of
                      Just _ -> False
                      Nothing -> True

isValid :: State -> Bool
isValid (State { program=program }) = (length (S.toList (S.fromList program))) == 3

buildInput :: State -> [Int]
buildInput (State { program=program, aPat=Just a, bPat=Just b, cPat=Just c }) =
  (commas program) ++ ([10]) ++ (commas a) ++ ([10]) ++ (commas b) ++ ([10]) ++ (commas c) ++ ([10]) ++ [ord 'n', ord '\n']
  where commas ss = map ord (intercalate "," ss)

main :: IO ()
main = do
  prog <- readFile "program.txt"
  solStr <- readFile "solution.txt"
  let sol = tokenize (filter (/='\n') solStr)
      initState = State { program=sol, aPat=Nothing, bPat=Nothing, cPat=Nothing }
      n1 = concat (map Main.next [initState])
      n2 = concat (map Main.next n1)
      n3 = concat (map Main.next n2)
      valid = filter isValid n3
      input = buildInput (head valid)
      computer = vmInitState prog 8192
      (_,o) = vmRunUntilMoreInputRequired computer input
  print $ o
