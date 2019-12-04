import Data.List.Split
import Data.List
import qualified Data.Set as Set
import System.Environment (getArgs)

data Direction = Up
               | Right
               | Down
               | Left
               deriving (Show)

data Length = Length Direction Int
              deriving (Show)

lengthFromString :: String -> Length
lengthFromString ('U':rest) = Length Main.Up (read rest)
lengthFromString ('R':rest) = Length Main.Right (read rest)
lengthFromString ('D':rest) = Length Main.Down (read rest)
lengthFromString ('L':rest) = Length Main.Left (read rest)

getPointsForWire :: [Length] -> [(Int, Int)]
getPointsForWire lengths =
  foldr moveWire [(0,0)] (reverse lengths)
  where moveWire l (p:ps) = (reverse (newPoints l p)) ++ p:ps
        newPoints (Length _ 0) p' = []
        newPoints (Length d n) p' = (apply d p'):(newPoints (Length d (n-1)) (apply d p'))
        apply Main.Up (x, y) = (x, y+1)
        apply Main.Right (x, y) = (x+1, y)
        apply Main.Down (x, y) = (x, y-1)
        apply Main.Left (x, y) = (x-1, y)

manhattanDistance :: (Int,Int) -> Int
manhattanDistance (x,y) = (abs x) + (abs y)

getIntersectionPoints :: [Length] -> [Length] -> [(Int,Int)]
getIntersectionPoints wire1 wire2 =
  Set.toList ((Set.fromList points1) `Set.intersection` (Set.fromList points2))
  where points1 = getPointsForWire wire1
        points2 = getPointsForWire wire2

getClosestIntersectionDistance :: [Length] -> [Length] -> Int
getClosestIntersectionDistance wire1 wire2 =
  minimum (filter (>0) (map manhattanDistance intersections))
  where intersections = getIntersectionPoints wire1 wire2

wrapper :: String -> String
wrapper input =
  show (getClosestIntersectionDistance wire1 wire2)
  where wire1 = map lengthFromString (splitOn "," l1)
        wire2 = map lengthFromString (splitOn "," l2)
        l1:l2:_ = lines input

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

        myFunction = wrapper
