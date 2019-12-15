module Main where

import Computer

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

tileTypeFromInt :: Int -> TileType
tileTypeFromInt n = case n of
                      0 -> EmptyTile
                      1 -> WallTile
                      2 -> BlockTile
                      3 -> HorizontalPaddleTile
                      4 -> BallTile

screenInsert :: Screen -> Int -> Int -> Int -> Screen
screenInsert s x y t =
  let tileType = tileTypeFromInt t
      tile = Tile x y tileType
      rest = filter (not . tileAtPosition (x,y)) s
  in (tile:rest)

screenApplyOutput :: Screen -> [Int] -> Screen
screenApplyOutput start output =
  case output of
    []              -> start
    (x1:x2:x3:rest) -> screenApplyOutput (screenInsert start x1 x2 x3) rest

data ArcadeCabinet = ArcadeCabinet { screen :: Screen, computer :: VmState }

createArcadeCabinet :: String -> ArcadeCabinet
createArcadeCabinet program = ArcadeCabinet { screen = [], computer = vmInitState program 4096 }

stepArcadeCabinet :: ArcadeCabinet -> ArcadeCabinet
stepArcadeCabinet start@(ArcadeCabinet { screen=screen, computer=computer }) =
  let (nextComputer, output) = vmRunUntilMoreInputRequired computer []
      nextScreen = screenApplyOutput screen output
  in ArcadeCabinet { computer=nextComputer, screen=nextScreen }

main :: IO ()
main = do
  prog <- readFile "program.txt"
  let game = createArcadeCabinet prog
      (ArcadeCabinet { screen=screen }) = stepArcadeCabinet game
      blockTiles = filter isBlockTile screen
  print $ length blockTiles
