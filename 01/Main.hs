import System.Environment

fuelRequiredApprox :: Int -> Int
fuelRequiredApprox mass = (mass `div` 3) - 2

fuelRequiredExact :: Int -> Int
fuelRequiredExact mass
  | fuelRequiredApprox mass <= 0  = 0
  | otherwise                     = (fuelRequiredApprox mass) + fuelRequiredExact (fuelRequiredApprox mass)

main = do
  input <- getContents
  args <- getArgs
  if not (null args) && head args == "--approx"
    then print $ sum (map fuelRequiredApprox (map (read :: String -> Int) (lines input)))
    else print $ sum (map fuelRequiredExact (map (read :: String -> Int) (lines input)))
