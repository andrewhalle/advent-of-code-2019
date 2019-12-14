{-# LANGUAGE DuplicateRecordFields #-}
module Main where

import Data.Char
import Data.List
import Data.List.Split

data Point = Point { x :: Int, y :: Int, z :: Int }
             deriving (Show)

data Velocity = Velocity { x :: Int, y :: Int, z :: Int }
                deriving (Show)

data Moon = Moon Point Velocity
            deriving (Show)

applyGravityPair :: (Moon, Moon) -> (Moon, Moon)
applyGravityPair (
  (Moon p1@(Point { x=x1, y=y1, z=z1 }) (Velocity { x=vx1, y=vy1, z=vz1 })),
  (Moon p2@(Point { x=x2, y=y2, z=z2 }) (Velocity { x=vx2, y=vy2, z=vz2 }))
  ) =
  let xdir = if x2 > x1 then 1 else if x2 < x1 then -1 else 0
      ydir = if y2 > y1 then 1 else if y2 < y1 then -1 else 0
      zdir = if z2 > z1 then 1 else if z2 < z1 then -1 else 0
  in (
    (Moon p1 (Velocity { x=(vx1+xdir), y=(vy1+ydir), z=(vz1+zdir) })),
    (Moon p2 (Velocity { x=(vx1-xdir), y=(vy2-ydir), z=(vz2-zdir) }))
  )

updatePositionSingle :: Moon -> Moon
updatePositionSingle
  (Moon (Point { x=x, y=y, z=z }) v@(Velocity { x=vx, y=vy, z=vz }))
  =
  (Moon (Point { x=(x+vx), y=(y+vy), z=(z+vz) }) v)

potentialEnergy :: Moon -> Int
potentialEnergy (Moon (Point { x=x, y=y, z=z }) _) = (abs x) + (abs y) + (abs z)

kineticEnergy :: Moon -> Int
kineticEnergy (Moon _ (Velocity { x=x, y=y, z=z })) = (abs x) + (abs y) + (abs z)

totalEnergy :: Moon -> Int
totalEnergy m = (potentialEnergy m) * (kineticEnergy m)

applyGravityAllPairs :: (Moon,[Moon]) -> Moon
applyGravityAllPairs (m,ms) = foldr (\x y -> (fst (applyGravityPair (y, x)))) m ms

idxAndRest :: [Moon] -> Int -> (Moon, [Moon])
idxAndRest moons idx =
  let moon = moons !! idx
      (pre,_:suf) = splitAt idx moons
  in (moon, pre ++ suf)

applyGravity :: [Moon] -> [Moon]
applyGravity ms =
  let idxs = take (length ms) [0..]
      paired = map (idxAndRest ms) idxs
  in map applyGravityAllPairs paired

updatePosition :: [Moon] -> [Moon]
updatePosition ms = map updatePositionSingle ms

step :: [Moon] -> [Moon]
step = (updatePosition . applyGravity)


parseInitial :: String -> [Moon]
parseInitial raw =
  let moons = lines raw
  in map parse moons
  where allowed c = (isDigit c) || (c == '-') || (c == ',')
        parse s = let f = filter allowed s
                      [x,y,z] = map (read :: String -> Int) (splitOn "," f)
                  in (Moon (Point { x=x, y=y, z=z }) (Velocity { x=0, y=0, z=0 }))

main :: IO ()
main = do
  init <- readFile "input.txt"
  let moons = parseInitial init
  let final = foldr (\_ y -> step y) moons [1..5977]
  print $ sum (map totalEnergy final)
