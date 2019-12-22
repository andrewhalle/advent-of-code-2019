module Main where

import Computer
import qualified Data.Set as S
import Data.List.Split

data RobotDirection = RobotUp
                    | RobotRight
                    | RobotDown
                    | RobotLeft
                    deriving (Show, Ord, Eq)

directionAsInt RobotUp = 1
directionAsInt RobotRight = 4
directionAsInt RobotDown = 2
directionAsInt RobotLeft = 3

invert RobotUp = RobotDown
invert RobotRight = RobotLeft
invert RobotDown = RobotUp
invert RobotLeft = RobotRight


invertDirections ds = map invert (reverse ds)

positionFromDirections :: [RobotDirection] -> (Int, Int)
positionFromDirections ds = foldr move (0, 0) ds
  where move d (x,y) = case d of
                        RobotUp -> (x,y+1)
                        RobotRight -> (x+1,y)
                        RobotDown -> (x,y-1)
                        RobotLeft -> (x-1,y)

directionsFromString :: String -> [RobotDirection]
directionsFromString s =
  let filtered = filter (/='\n') s
      ds = splitOn "," filtered
  in map convert ds
  where convert s' = case s' of
                       "RobotUp" -> RobotUp
                       "RobotRight" -> RobotRight
                       "RobotDown" -> RobotDown
                       "RobotLeft" -> RobotLeft

data RobotResult = RobotSuccess
                 | RobotWall
                 | RobotFoundOxygen

-- implements BFS given the constraints that can't store state
-- need to generate states by walking back and forth
data BFSState = BFSState { running :: Bool, queue :: [[RobotDirection]], seen :: S.Set (Int,Int), computer :: VmState, longest :: Int }
                deriving (Show)

search :: VmState -> [RobotDirection]
search computer =
  let startState = BFSState { running=True, queue=[[RobotUp], [RobotRight], [RobotDown], [RobotLeft]], seen=S.insert (0,0) S.empty, computer=computer, longest=0 }
      endState@(BFSState { queue=(found:_)}) = head (dropWhile running (iterate bfsStep startState))
  in found
  where running (BFSState {running=r}) = r

search2 :: VmState -> BFSState
search2 computer =
  let startState = BFSState { running=True, queue=[[RobotUp], [RobotRight], [RobotDown], [RobotLeft]], seen=S.insert (0,0) S.empty, computer=computer, longest=0 }
      endState = head (dropWhile running (iterate bfsStep startState))
  in endState
  where running (BFSState {running=r}) = r

bfsStep :: BFSState -> BFSState
bfsStep curr@(BFSState { running=False }) = curr
bfsStep curr@(BFSState { running=True, queue=[] }) = curr { running=False }
bfsStep curr@(BFSState { running=True, queue=queue, seen=seen, computer=computer, longest=longest }) =
  let toTest = head queue
      alreadySeen = S.member (positionFromDirections toTest) seen
  in if alreadySeen
     then curr { queue=(tail queue)}
     else let (c1, output) = vmRunUntilMoreInputRequired computer (map directionAsInt toTest)
              result = last output
              finished = result == 2
              addNeighbors = result == 1
              isLonger = (length toTest) > longest
              newLongest = if isLonger then length toTest else longest
              neighbors = if addNeighbors then [toTest ++ [RobotUp], toTest ++ [RobotRight], toTest ++ [RobotDown], toTest ++ [RobotLeft]] else []
              (c2, _) = vmRunUntilMoreInputRequired computer (map directionAsInt (invertDirections toTest))
          in if finished
             then (BFSState { running=False, queue=queue, seen=seen, computer=c2, longest=newLongest })
             else (BFSState { running=True, queue=((tail queue) ++ neighbors), seen=(S.insert (positionFromDirections toTest) seen), computer=c2, longest=newLongest })


main :: IO ()
main = do
  prog <- readFile "program.txt"
  pathStr <- readFile "path-to-oxygen.txt"
  let path = directionsFromString pathStr
  let c1 = vmInitState prog 2048
      (c2,_) = vmRunUntilMoreInputRequired c1 (map directionAsInt path)
      BFSState { longest=longest } = search2 c2
  print $ longest
