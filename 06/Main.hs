import System.Environment (getArgs)
import Data.List.Split
import qualified Data.HashMap.Strict as H
import qualified Data.Set as S

-- utils

twoListToTup :: [String] -> (String, String)
twoListToTup (l1:l2:[]) = (l1, l2)
twoListToTup _ = error "list must be of length 2"

-- end utils

-- Node has a list of neighbor nodeIds
data Node = Node [String]
            deriving (Show)

data BfsState = Running [(String, Int)] (S.Set String)
              | Finished Int
              deriving (Show)

-- split pair-string on ')'
parsePair :: String -> [String]
parsePair p = splitOn ")" p

-- inserts the given edge to the map
insertEdge :: H.HashMap String Node -> (String, String) -> H.HashMap String Node
insertEdge graph (orbitee,orbiter) =
  let (Node n1Neighbors)  = H.lookupDefault (Node []) orbitee graph
      (Node n2Neighbors)  = H.lookupDefault (Node []) orbiter graph
      n1New               = (Node (orbiter:n1Neighbors))
      n2New               = (Node (orbitee:n2Neighbors))
  in H.insert orbitee n1New (H.insert orbiter n2New graph)

-- given list of edges, return hashmap representing graph
parseGraph :: [String] -> H.HashMap String Node
parseGraph strPairs =
  let pairs = (((map twoListToTup) . (map parsePair)) strPairs) :: [(String,String)]
      initMap = H.empty
  in foldl insertEdge initMap pairs

-- find shortest path between nodes using BFS
shortestPathLength :: H.HashMap String Node -> String -> String -> Int
shortestPathLength graph startId endId =
  let initState = (Running [(startId, 0)] S.empty)
  in bfs initState
  where bfs state = case state of
                      Finished n            -> n
                      Running queue visited -> bfs (nextState queue visited)
        nextState (x:xs) visited = if (fst x) == endId
                                   then Finished (snd x)
                                   else if (S.member (fst x) visited)
                                        then Running xs visited
                                        else Running (xs ++ (neighbors (fst x) ((snd x) + 1))) (S.insert (fst x) visited)
        neighbors nodeId dist = let (Node ns) = (H.lookupDefault (Node []) nodeId graph)
                                in map (\id -> (id, dist)) ns

_main :: String -> String
_main input =
  let graph = (parseGraph . lines) input
  in show ((shortestPathLength graph "YOU" "SAN") - 2)

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

        myFunction = (\_ -> "hello world")
