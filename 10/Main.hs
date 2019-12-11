module Main where

import Data.Ratio
import Data.List
import System.Environment

parseAsteroidPositions :: String -> [(Int,Int)]
parseAsteroidPositions raw =
  let width = (length . (takeWhile (/='\n'))) raw
      enumerated = zip [0..] (filter (/='\n') raw)
      asteroids = filter isAsteroid enumerated
      positions = map (getPosition width) asteroids
  in positions
  where isAsteroid x = case x of
                         (_, '#') -> True
                         (_, '.') -> False
                         _        -> error "invalid character"
        getPosition width (idx, _) = (idx `mod` width, idx `div` width)

visibleAsteroids :: (Int,Int) -> [(Int,Int)] -> Int
visibleAsteroids pos asteroids =
  let asteroidsWithoutPos = filter (/=pos) asteroids
      directions = map (direction pos) asteroidsWithoutPos
      uniqueDirections = nubBy (\x y -> (flEq (fst x) (fst y)) && (flEq (snd x) (snd y))) directions
  in length uniqueDirections
  where direction origin@(x1,y1) target@(x2,y2) = norm (x2-x1,y2-y1)
        dist x y = sqrt (fromIntegral (x*x + y*y))
        norm (x,y) = ((fromIntegral x)/(dist x y),(fromIntegral y)/(dist x y))
        epln = 1e-9
        flEq x y = (abs (x - y)) <= epln

bestLocation :: [(Int,Int)] -> (Int,(Int,Int))
bestLocation asteroids =
  let visible = map calcVisible asteroids
  in  maximumBy (\x y -> compare (fst x) (fst y)) visible
  where calcVisible loc = (visibleAsteroids loc asteroids, loc)

main :: IO ()
main = do
  [starMapFile] <- getArgs
  starMap <- readFile starMapFile
  print $ bestLocation (parseAsteroidPositions starMap)
