module Main where

import Computer
import System.IO
import Data.List.Split

data JoystickInput = LeftJoystick
                   | NeutralJoystick
                   | RightJoystick

joystickInputAsInt :: JoystickInput -> Int
joystickInputAsInt LeftJoystick = -1
joystickInputAsInt NeutralJoystick = 0
joystickInputAsInt RightJoystick = 1

data TileType = EmptyTile
              | WallTile
              | BlockTile
              | HorizontalPaddleTile
              | BallTile

data Tile = Tile Int Int TileType
type Screen = [Tile]

tileAtPosition :: (Int,Int) -> Tile -> Bool
tileAtPosition p (Tile x y _) = p == (x,y)

isBlockTile :: Tile -> Bool
isBlockTile (Tile _ _ BlockTile) = True
isBlockTile (Tile _ _ _) = False

isBallTile :: Tile -> Bool
isBallTile (Tile _ _ BallTile) = True
isBallTile (Tile _ _ _) = False

isHorizontalPaddleTile :: Tile -> Bool
isHorizontalPaddleTile (Tile _ _ HorizontalPaddleTile) = True
isHorizontalPaddleTile (Tile _ _ _) = False

tileTypeFromInt :: Int -> TileType
tileTypeFromInt n = case n of
                      0 -> EmptyTile
                      1 -> WallTile
                      2 -> BlockTile
                      3 -> HorizontalPaddleTile
                      4 -> BallTile

tileGetXCoordinate :: Tile -> Int
tileGetXCoordinate (Tile x _ _) = x

tileGetYCoordinate :: Tile -> Int
tileGetYCoordinate (Tile _ y _) = y

screenInsert :: Screen -> Int -> Int -> Int -> Screen
screenInsert s x y t =
  let tileType = tileTypeFromInt t
      tile = Tile x y tileType
      rest = filter (not . tileAtPosition (x,y)) s
  in (tile:rest)


data ArcadeCabinet = ArcadeCabinet { screen :: Screen, computer :: VmState, score :: Int }

createArcadeCabinet :: String -> ArcadeCabinet
createArcadeCabinet program = ArcadeCabinet { screen = [], computer = vmInitState program 4096, score = 0 }

arcadeCabinetApplyOutput :: ArcadeCabinet -> [Int] -> ArcadeCabinet
arcadeCabinetApplyOutput start output =
  case output of
    []                -> start
    (-1:0:score:rest) -> arcadeCabinetApplyOutput (start { score=score }) rest
    (x1:x2:x3:rest) -> arcadeCabinetApplyOutput (start { screen=(screenInsert (screen start) x1 x2 x3) }) rest

stepArcadeCabinet :: ArcadeCabinet -> JoystickInput -> ArcadeCabinet
stepArcadeCabinet start@(ArcadeCabinet { screen=screen, computer=computer }) joystick =
  let (nextComputer, output) = vmRunUntilMoreInputRequired computer [joystickInputAsInt joystick]
      nextGame = arcadeCabinetApplyOutput start output
  in nextGame { computer=nextComputer }

arcadeWidth :: ArcadeCabinet -> Int
arcadeWidth (ArcadeCabinet { screen=screen }) =
  let xs = map tileGetXCoordinate screen
  in maximum xs

arcadeHeight :: ArcadeCabinet -> Int
arcadeHeight (ArcadeCabinet { screen=screen }) =
  let ys = map tileGetYCoordinate screen
  in maximum ys

arcadeOutputAsString :: ArcadeCabinet -> String
arcadeOutputAsString a@(ArcadeCabinet { screen=screen, score=score }) =
  let width = arcadeWidth a
      height = arcadeHeight a
      tiles = take (width*height) (foldr (buildString width screen) "" [0..])
      ls = chunksOf width tiles
      s = unlines ls
  in s ++ ['\n'] ++ "Score: " ++ (show score)
  where buildString width ts idx rest =
          let x = idx `mod` width
              y = idx `div` width
              [(Tile _ _ t)] = filter (tileAtPosition (x,y)) ts
              c = case t of
                    EmptyTile -> ' '
                    WallTile -> '|'
                    BlockTile -> '#'
                    HorizontalPaddleTile -> '='
                    BallTile -> 'o'
          in (c:rest)

gameStillRunning :: ArcadeCabinet -> Bool
gameStillRunning (ArcadeCabinet { computer=computer }) =
  case computer of
    Running _ _ _ -> True
    _             -> False

getBallPosition :: ArcadeCabinet -> (Int,Int)
getBallPosition (ArcadeCabinet { screen=screen }) =
  let ts = filter isBallTile screen
      (Tile x y _) = case ts of
                       [] -> Tile 0 0 EmptyTile
                       [t] -> t
  in (x,y)

getPaddlePosition :: ArcadeCabinet -> (Int,Int)
getPaddlePosition (ArcadeCabinet { screen=screen }) =
  let ts = filter isHorizontalPaddleTile screen
      (Tile x y _) = case ts of
                       [] -> Tile 0 0 EmptyTile
                       [t] -> t
  in (x,y)

runGame :: ArcadeCabinet -> IO ()
runGame curr = do
  -- j <- getChar
  -- let joystickInput = case j of
  --                       'a' -> LeftJoystick
  --                       's' -> NeutralJoystick
  --                       'd' -> RightJoystick
  let (ballX,_) = getBallPosition curr
      (paddleX,_) = getPaddlePosition curr
      joystickInput = if paddleX == ballX
                      then NeutralJoystick
                      else if paddleX > ballX
                           then LeftJoystick
                           else RightJoystick
      newGame = stepArcadeCabinet curr joystickInput
  putStrLn (arcadeOutputAsString newGame)
  if gameStillRunning newGame
  then runGame newGame
  else return ()

main :: IO ()
main = do
  hSetBuffering stdin NoBuffering
  prog <- readFile "program.txt"
  let game = createArcadeCabinet prog
  runGame game
