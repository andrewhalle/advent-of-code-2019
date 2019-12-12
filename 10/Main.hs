module Main where

import Data.Ratio
import Data.List
import System.Environment

data Asteroid = Asteroid { distance :: Double, angle :: Double, pos :: (Int,Int) }
                deriving (Show)

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

calcAngle :: (Int,Int) -> (Int,Int) -> Double
calcAngle origin@(x1,y1) p@(x2,y2) =
  let xdiff = (fromIntegral (x2 - x1)) :: Double
      ydiff = (fromIntegral (y1 - y2)) :: Double -- flip y because positive y points down
      angle' = atan2 ydiff xdiff
      angle'' = (pi / 2) - angle'
      angle = if angle'' < 0.0 then angle'' + (2 * pi) else angle''
  in angle

calcDistance :: (Int,Int) -> (Int,Int) -> Double
calcDistance origin@(x1,y1) p@(x2,y2) = sqrt ( (fromIntegral (x2 - x1)^2) + (fromIntegral (y2 - y1)^2))

constructAsteroids :: (Int,Int) -> [(Int,Int)] -> [Asteroid]
constructAsteroids station positions =
  let positionsExcludingStation = filter (/= station) positions
  in map (\x -> Asteroid { distance = calcDistance station x, angle = calcAngle station x, pos = x }) positionsExcludingStation

flEq :: Double -> Double -> Bool
flEq a b =
  let epsln = 1E-9
  in (abs (a - b)) <= epsln

bucketByAngle :: [Asteroid] -> [(Double,[Asteroid])]
bucketByAngle asteroids =
  foldr insertIntoBucket [] asteroids
  where insertIntoBucket asd buckets =
          let bucket = filter (flEq (angle asd) . fst) buckets
              newBucket = case bucket of
                            []       -> (angle asd, [asd])
                            [(_,as)] -> (angle asd, asd:as)
              rest = filter (not . flEq (angle asd) . fst) buckets
          in (newBucket:rest)

sortBuckets :: [(Double,[Asteroid])] -> [(Double,[Asteroid])]
sortBuckets = sortBy (\x y -> compare (fst x) (fst y))

main :: IO ()
main = do
  [starMapFile] <- getArgs
  starMap <- readFile starMapFile
  print $ ((sortBuckets . bucketByAngle . (constructAsteroids (11,13)) . parseAsteroidPositions) starMap) !! 199
