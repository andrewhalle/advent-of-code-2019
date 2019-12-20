module Main where

import qualified Data.HashMap as H
import Data.List
import Data.List.Split

testRules :: H.Map String (Int, [(String, Int)])
testRules =
  let h1 = H.empty
      h2 = H.insert "A" (10, [("ORE", 10)]) h1
      h3 = H.insert "B" (1, [("ORE", 1)]) h2
      h4 = H.insert "C" (1, [("A", 7), ("B", 1)]) h3
      h5 = H.insert "D" (1, [("A", 7), ("C", 1)]) h4
      h6 = H.insert "E" (1, [("A", 7), ("D", 1)]) h5
      h7 = H.insert "FUEL" (1, [("A", 7), ("E", 1)]) h6
  in h7

-- invert an amount of a chemical into its component parts
invert :: H.Map String (Int, [(String, Int)]) -> (String, Int) -> [(String, Int)]
invert rules orig@("ORE", amt) = [orig]
invert rules (result, amt) =
  let Just rule = H.lookup result rules
      amtProduced = fst rule
      components = snd rule
      productionFactor = if amtProduced >= amt then 1 else ceiling ((fromIntegral amt) / (fromIntegral amtProduced))
  in scaleReactants productionFactor components

scaleReactants :: Int -> [(String, Int)] -> [(String, Int)]
scaleReactants n rs = map (\x -> (fst x, (snd x) * n)) rs

combineLikeReactants :: [(String, Int)] -> [(String, Int)]
combineLikeReactants initial = H.toList (foldr (\x y -> addToMap y x) H.empty initial)
  where addToMap m (name, val) =
          let currVal = H.findWithDefault 0 name m
          in H.insert name (currVal + val) m

-- take a list and keep inverting until only ORE is left
-- we have to invert in reverse topologically sorted order
invertToOre :: [String] -> H.Map String (Int, [(String, Int)]) -> [(String, Int)] -> [(String, Int)]
invertToOre ordering rules curr =
  let inverter = invert rules
      nameToInvert = head ordering
      ([toInvert], rest) = partition (\x -> (fst x) == nameToInvert) curr
      inverted = inverter toInvert
      reduced = combineLikeReactants (inverted ++ rest)
  in case ordering of
       [x]    -> reduced
       (x:xs) -> invertToOre xs rules reduced

-- need to top-sort rules for invertToOre
topSort :: H.Map String (Int, [(String, Int)]) -> [String]
topSort graph = ("FUEL"):(topSortHelper graph "FUEL" [])

topSortHelper :: H.Map String (Int, [(String, Int)]) -> String -> [String] -> [String]
topSortHelper graph curr startStack =
  let Just (_, ns) = H.lookup curr graph
      neighbors = filter (/= "ORE") (map fst ns)
      newStack = foldr (\n s -> if (n `elem` s) then s else n:(topSortHelper graph n s)) startStack neighbors
  in newStack

-- now, just need to parse the rules graph from the input string
parseRules :: String -> H.Map String (Int, [(String, Int)])
parseRules raw =
  let ls = lines raw
  in foldr parseRule H.empty ls
  where parseRule s h =
          let [ins1, outs1] = splitOn " => " s
              [outAmt, outName] = splitOn " " outs1
              ins = map (\x -> let [inAmt, inName] = splitOn " " x
                               in (inName, read inAmt)
                        ) (splitOn ", " ins1)
              val = (read outAmt, ins)
          in H.insert outName val h

main :: IO ()
main = do
  raw <- readFile "puzzle.txt"
  let rules = parseRules raw
  print $ invertToOre (topSort rules) rules [("FUEL", 998536)]
