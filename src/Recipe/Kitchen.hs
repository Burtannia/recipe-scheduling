{-# LANGUAGE RecordWildCards #-}

module Recipe.Kitchen where

import Recipe.Recipe
import Recipe.Helper
import Algebra.Graph
import Data.Maybe (isJust)

data Station = Station
    { stName :: String
    , stFunc :: Action -> [Action] -> Maybe Time
    , stCapacity :: Int -- number of concurrent actions
    }

data Env = Env [Station]

validStations :: IxAction -> Recipe -> Env -> [Station]
validStations a r (Env sts) = filter (isValidSt (snd a) (map snd $ deps a r)) sts

isValidSt :: Action -> [Action] -> Station -> Bool
isValidSt a ds Station {..} = isJust $ stFunc a ds

fromCapacity :: [Station] -> [Station]
fromCapacity = concatMap $ \Station {..} ->
    [ Station (stName ++ '_' : show i) stFunc 1 | i <- [1 .. stCapacity] ]

-- start with ovens of set temperature
-- multiply stations based on capacity
-- get list of valid stations for each action