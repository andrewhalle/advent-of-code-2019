module Main where

import Computer
import System.Environment
import Data.List

data Color = Black
           | White
           deriving (Show, Eq)

data HullSquare = HullSquare { hpos :: (Int,Int), color :: Color }
                  deriving (Show)

hullSquareAt :: (Int,Int) -> HullSquare -> Bool
hullSquareAt p1 (HullSquare { hpos=p2 }) = p1 == p2

hullInsert :: [HullSquare] -> (Int,Int) -> Color -> [HullSquare]
hullInsert sqs pos c =
  let rest = filter (not . hullSquareAt pos) sqs
  in (HullSquare { hpos = pos, color = c }):rest

data RobotDirection = RobotUp
                    | RobotRight
                    | RobotDown
                    | RobotLeft
                    deriving (Show)

directionNext :: RobotDirection -> (Int,Int) -> (Int,Int)
directionNext dir (x,y) = case dir of
                            RobotUp    -> (x,y+1)
                            RobotRight -> (x+1,y)
                            RobotDown  -> (x,y-1)
                            RobotLeft  -> (x-1,y)

data TurnDirection = TurnLeft
                   | TurnRight
                   deriving (Show)

turnNext :: TurnDirection -> RobotDirection -> RobotDirection
turnNext TurnLeft facing = case facing of
                             RobotUp    -> RobotLeft
                             RobotRight -> RobotUp
                             RobotDown  -> RobotRight
                             RobotLeft  -> RobotDown
turnNext TurnRight facing = case facing of
                              RobotUp    -> RobotRight
                              RobotRight -> RobotDown
                              RobotDown  -> RobotLeft
                              RobotLeft  -> RobotUp

data Robot = Robot { computer :: VmState, pos :: (Int,Int), facing :: RobotDirection, hull :: [HullSquare]}
             deriving (Show)

newRobot :: String -> Robot
newRobot program = Robot { computer = vmInitState program 2048, pos = (0,0), facing = RobotUp, hull = [(HullSquare { hpos=(0,0), color=White })] }

robotGetCurrentColor :: Robot -> Color
robotGetCurrentColor (Robot { pos=pos, hull=hull }) = case (filter (hullSquareAt pos) hull) of
                                                        []                             -> Black
                                                        [(HullSquare { color=color })] -> color

robotPaint :: Robot -> Color -> Robot
robotPaint r@(Robot { pos=pos, hull=hull }) c = r { hull = hullInsert hull pos c }


robotStep :: Robot -> Robot
robotStep r@(Robot { pos=pos, facing=facing }) = r { pos = directionNext facing pos }

robotTurn :: Robot -> TurnDirection -> Robot
robotTurn r@(Robot { facing=facing }) turn = r { facing = turnNext turn facing }

robotRun :: Robot -> Robot
robotRun curr@(Robot { computer=computer }) =
  case computer of
    Finished _    -> curr
    Error _       -> curr
    Running _ _ _ -> let (newComp, [newColor,turn]) = vmRunUntilMoreInputRequired computer [ if (robotGetCurrentColor curr) == Black then 0 else 1]
                         r1                         = robotPaint curr (if newColor == 0 then Black else White)
                         r2                         = robotTurn r1 (if turn == 0 then TurnLeft else TurnRight)
                         r3                         = robotStep r2
                         r4                         = r3 { computer = newComp }
                     in r4

robotRunUntilFinished :: Robot -> Robot
robotRunUntilFinished start =
  let next@(Robot { computer=computer }) = robotRun start
  in case computer of
       Running _ _ _ -> robotRunUntilFinished next
       _             -> next

robotGetHullAsString :: Robot -> String
robotGetHullAsString (Robot { hull=hull }) =
  let (HullSquare { hpos=(x1,_) }) = minimumBy (\x y -> compare (getX x) (getX y)) hull
      (HullSquare { hpos=(x2,_) }) = maximumBy (\x y -> compare (getX x) (getX y)) hull
      (HullSquare { hpos=(_,y1) }) = minimumBy (\x y -> compare (getY x) (getY y)) hull
      (HullSquare { hpos=(_,y2) }) = maximumBy (\x y -> compare (getY x) (getY y)) hull
      width = x2 - x1
      height = y2 - y1
      s1 = take ((width+4) * (height+4)) (foldr (buildString x1 y2 width height hull) "" [0..])
      s2 = lined width s1
  in s2
  where buildString l t w h hs idx curr =
          let p = ((l-1) + (idx `mod` w), (t+1) - (idx `div` w))
              sq = filter (hullSquareAt p) hs
              c = case sq of
                  [s] -> color s
                  _   -> Black
          in case c of
               Black -> ((toEnum 9608):curr)
               White -> (' ':curr)
        getX = fst . hpos
        getY = snd . hpos
        lined w s = if (length s) <= w then s else (take w s) ++ ('\n':(lined w (drop w s)))


main :: IO ()
main = do
  [progFile] <- getArgs
  prog <- readFile progFile
  let r = robotRunUntilFinished (newRobot prog)
      s1 = robotGetHullAsString r
  putStrLn s1
  print $ (length . hull) r
