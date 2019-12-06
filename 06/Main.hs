import System.Environment (getArgs)
import Data.List.Split
import qualified Data.HashMap.Strict as H
import qualified Data.Set as Set

-- utils

twoListToTup :: [String] -> (String, String)
twoListToTup (l1:l2:[]) = (l1, l2)
twoListToTup _ = error "list must be of length 2"

-- end utils

-- Node has a possible next node, and stores the number of
-- nodes that orbit it
data Node = Node (Maybe String) Int

-- split pair-string on ')'
parsePair :: String -> [String]
parsePair p = splitOn ")" p

nodeNames :: [String] -> [String]
nodeNames pairs = (Set.toList . Set.fromList . concat . (map parsePair)) pairs

-- increment counter of node and all its parents by one
incrementCounters :: H.HashMap String Node -> String -> Int -> H.HashMap String Node
incrementCounters map nodeId amount =
  let (Node parent countOrbitting) = H.lookupDefault (Node Nothing 0) nodeId map
      newNode = (Node parent (countOrbitting+(amount+1)))
  in case parent of
       Just parentId -> incrementCounters (H.insert nodeId newNode map) parentId amount
       Nothing       -> H.insert nodeId newNode map

-- inserts the given edge to the map
insertEdge :: H.HashMap String Node -> (String, String) -> H.HashMap String Node
insertEdge map (orbitee,orbiter) =
  let to                                = H.lookupDefault (Node Nothing 0) orbitee map
      from@(Node parent countOrbitting) = H.lookupDefault (Node Nothing 0) orbiter map
      newNode = case parent of
                  Nothing -> Node (Just orbitee) countOrbitting
                  Just _  -> error "node can't have 2 parents"
  in incrementCounters (H.insert orbiter newNode map) orbitee countOrbitting

-- given a list of edges, parses a DAG and returns the head node
-- works because we know the name of the head node ahead of time (COM)
parseDAG :: [String] -> ([String], H.HashMap String Node)
parseDAG strPairs =
  let nodes = nodeNames strPairs
      pairs = (((map twoListToTup) . (map parsePair)) strPairs) :: [(String,String)]
      initMap = H.empty
  in (nodes, (foldl insertEdge initMap pairs))

sumAllCounts :: [String] -> H.HashMap String Node -> Int
sumAllCounts nodes hmap =
  sum (map getCount nodes)
  where getNode node = (H.lookupDefault (Node Nothing 0) node hmap)
        getCount node = let (Node _ count) = getNode node
                        in count

_main :: String -> String
_main input =
  let (nodes, map) = (parseDAG . lines) input
  in show (sumAllCounts nodes map)

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
