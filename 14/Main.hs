module Main where

import qualified Data.Set as S

-- current state. we always have an unlimited amount of ore at our
-- disposal, and we've used a certain amount of ore to get here
data State = State { materials :: [(String, Int)], oreUsed :: Int }
             deriving (Show)

-- helper function for updating the count of one chemical
updateMaterials :: [(String, Int)] -> String -> Int -> [(String, Int)]
updateMaterials curr chem delta =
  let toUpdate    = lookup chem curr
      currAmount  = case toUpdate of
                      Just n  -> n
                      Nothing -> 0
      new         = if (currAmount + delta) < 0
                    then error "not enough materials"
                    else (chem, currAmount + delta)
      rest        = filter (\x -> (fst x) /= chem) curr
  in (new:rest)

-- the starting state for the search
initialState = State { materials=[], oreUsed=0 }

-- how to produce a new chemical given some existing ones
data Recipe = Recipe { inputs :: [(String, Int)], output :: (String, Int) }
              deriving (Show)

-- do we have enough materials to apply this recipe?
canApplyRecipe :: State -> Recipe -> Bool
canApplyRecipe (State { materials=materials }) (Recipe { inputs=inputs }) =
  and (map haveEnough inputs)
  where haveEnough (chem, req) =
          if chem == "ORE" then True
          else let have = lookup chem materials
          in case have of
            Just n  -> n >= req
            Nothing -> False

-- apply recipe returning a new state
applyRecipe :: State -> Recipe -> State
applyRecipe
  s@(State { materials=materials, oreUsed=oreUsed })
  r@(Recipe { inputs=inputs, output=output })
  =
  if not (canApplyRecipe s r) then error "can't apply that recipe"
  else let oreReq = lookup "ORE" inputs
           newOreUsed = case oreReq of
                          Just n  -> oreUsed+n
                          Nothing -> oreUsed
           toUpdate = output:(map (\x -> (fst x, -(snd x))) (filter (\x -> (fst x) /= "ORE") inputs))
           newMaterials = filter (\x -> (snd x) /= 0) (foldr f materials toUpdate)
       in State { materials=newMaterials, oreUsed=newOreUsed }
       where f (c, a) curr = updateMaterials curr c a



main :: IO ()
main = putStrLn "Hello, Haskell!"
