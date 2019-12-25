module Main where

import Data.Char
import Data.List
import qualified Data.Set as S

data Tile = EmptyTile
          | WallTile
          | KeyTile Char
          | LockedDoorTile Char
          deriving (Show,Eq,Ord)

-- parse tile from character
parseTile :: Char -> Tile
parseTile c = case c of
                '#' -> WallTile
                '.' -> EmptyTile
                '@' -> EmptyTile
                _   -> if isUpper c
                       then LockedDoorTile c
                       else KeyTile c

tileIsEmpty :: Tile -> Bool
tileIsEmpty EmptyTile = True
tileIsEmpty _ = False

tileIsKey :: Tile -> Bool
tileIsKey (KeyTile _) = True
tileIsKey _ = False

data Maze = Maze [Tile] (Int, Int)
            deriving (Show,Eq,Ord)

-- get a tile from the list
getTile :: Maze -> (Int,Int) -> Tile
getTile (Maze ts (w,h)) (x,y) =
  let idx = (y * w) + x
  in ts !! idx

posOfIdx :: Maze -> Int -> (Int,Int)
posOfIdx (Maze _ (w,_)) idx = (idx `mod` w, idx `div` w)

-- if tile is correct door, return empty tile
-- else return tile unchanged
tileUnlock :: Char -> Tile -> Tile
tileUnlock d t = case t of
                   LockedDoorTile d' -> if (toUpper d) == d'
                                        then EmptyTile
                                        else t
                   KeyTile d'        -> if d == d'
                                        then EmptyTile
                                        else t
                   _                 -> t

-- is the provided position within the dimensions
-- and either empty or a key?
isValidPosition :: Maze -> (Int,Int) -> Bool
isValidPosition m@(Maze _ (w,h)) p@(x,y) =
  x >= 0 &&
    x < w &&
    y >= 0 &&
    y < h &&
    (tileIsEmpty (getTile m p) ||
      tileIsKey (getTile m p))

-- remove a door for key
mazeUnlock :: Maze -> Char -> Maze
mazeUnlock (Maze ts d) k = Maze (map (tileUnlock k) ts) d

-- parse maze from puzzle input
mazeFromString :: String -> Maze
mazeFromString raw =
  let Just width = '\n' `elemIndex` raw
      height = length (lines raw)
      parsed = map parseTile (filter (/='\n') raw)
  in Maze parsed (width,height)

data State = State { maze :: Maze, pos :: (Int, Int), steps :: Int }
             deriving (Show)

instance Eq State where
  (State { maze=m1, pos=p1 }) == (State { maze=m2, pos=p2 }) = (m1 == m2) && (p1 == p2)


stateUpdatePosition :: State -> (Int,Int) -> State
stateUpdatePosition (State { maze=maze, steps=steps }) pos =
  let t = getTile maze pos
      newMaze = case t of
                  KeyTile d -> mazeUnlock maze d
                  _         -> maze
  in State { maze=newMaze, pos=pos, steps=(steps+1) }

-- possible next states
neighbors :: State -> [State]
neighbors curr@(State { maze=maze, pos=(x,y)}) =
  let options = [(x+1,y), (x-1,y), (x,y+1), (x,y-1)]
      possible = filter (isValidPosition maze) options
  in map (stateUpdatePosition curr) possible

-- parse initial state from puzzle input
stateFromString :: String -> State
stateFromString raw =
  let maze@(Maze _ (width,height)) = mazeFromString raw
      Just idx = '@' `elemIndex` (filter (/='\n') raw)
      pos = (idx `mod` width, idx `div` width)
  in State { maze=maze, pos=pos, steps=0 }

-- have we collected all the keys?
goalTest :: State -> Bool
goalTest (State { maze=(Maze ts _)}) = (length (filter tileIsKey ts)) == 0

manhattanDistance :: (Int,Int) -> (Int,Int) -> Int
manhattanDistance (x1,y1) (x2,y2) = (abs (x2-x1)) + (abs (y2-y1))

keyDistancesFromPos :: State -> [Int]
keyDistancesFromPos (State { maze=maze, pos=pos }) =
  let (Maze ts _) = maze
      ks = filter (\x -> tileIsKey (fst x)) (zip ts [0..])
      ps = map (posOfIdx maze) (map snd ks)
  in map (manhattanDistance pos) ps

stateFarthestKey :: State -> Int
stateFarthestKey s =
  let ds = keyDistancesFromPos s
  in if null ds then 0 else maximum ds

-- bfs
stateStep :: [State] -> [State]
stateStep (x:xs) = sortBy (\x y -> compare ((sum (keyDistancesFromPos x)) + (steps x)) ((sum (keyDistancesFromPos y)) + (steps y))) (xs ++ (neighbors x))

bfsStep :: ([State], [State]) -> ([State], [State])
bfsStep (seen,ss) = if (head ss) `elem` seen
                  then (seen, tail ss)
                  else ((head ss):seen, stateStep ss)

main :: IO ()
main = do
  raw <- readFile "test03.txt"
  let s = stateFromString raw
      queue = [s]
      bfsState = ([], queue)
      solution = (head . snd . head) (dropWhile (not . goalTest . head . snd) (iterate bfsStep bfsState))
  print $ solution
