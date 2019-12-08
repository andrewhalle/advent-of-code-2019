module Main where

import System.Environment
import Data.List
import Data.Char

-- given width and height of layers (and data) return
-- list of layers
splitLayers :: Int -> Int -> [Int] -> [[Int]]
splitLayers _ _ [] = []
splitLayers width height img =
  let layerSize = width * height
      firstLayer = take layerSize img
      rest = drop layerSize img
  in firstLayer:(splitLayers width height rest)

-- visiblness
visible :: (Int,Int) -> Int
visible (t,b) = if t == 2
                then b
                else t

-- combine two layers
combineLayers :: [Int] -> [Int] -> [Int]
combineLayers top bottom = map visible (zip top bottom)

-- create final image
createImage :: [[Int]] -> [Int]
createImage layers@(first:_) = foldl combineLayers (replicate (length first) 2) layers

pixelToChar :: Int -> Char
pixelToChar 0 = toEnum 9608
pixelToChar 1 = ' '
pixelToChar _ = error "invalid pixel"

-- turn image into string to print on terminal
imgToString :: Int -> [Int] -> String
imgToString _ [] = ""
imgToString width img =
  let line = take width img
      lineStr = map pixelToChar line
      rest = drop width img
  in lineStr ++ ('\n':(imgToString width rest))

main :: IO ()
main = do
  [inputFilename] <- getArgs
  input <- readFile inputFilename
  let width = 25
      height = 6
      numStrs = ((map (\x -> [x])) . (filter (\x -> (x >= '0') && (x <= '9')))) input
      nums = map (read :: String -> Int) numStrs
      layers = splitLayers width height nums
      img = createImage layers
      imgStr = imgToString width img
  print $ layers
  print $ img
  putStrLn imgStr
