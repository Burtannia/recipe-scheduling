{-# LANGUAGE RecordWildCards #-}

module Recipe.Kitchen where

import Recipe.Recipe
import Recipe.Helper
import Algebra.Graph
import Data.Maybe (isJust, catMaybes)
import Data.List (sortBy)

data Station = Station
    { stName :: String -- should be unique
    , stFunc :: Action -> [Action] -> Maybe Time
    , stCapacity :: Int -- number of concurrent actions
    }

instance Eq Station where
    s1 == s2 = (stName s1) == (stName s2)

instance Ord Station where
    compare s1 s2 = compare (stName s1) (stName s2)

data Env = Env [Station]

validStations :: IxAction -> Recipe -> Env -> [(String, Time)]
validStations a r (Env sts) = catMaybes $
    flip map sts $ \Station {..} ->
        fmap (\time -> (stName, time)) $
            stFunc (snd a) (map snd $ deps a r)

isValidSt :: Action -> [Action] -> Station -> Bool
isValidSt a ds Station {..} = isJust $ stFunc a ds

fromCapacity :: Env -> Env
fromCapacity (Env sts) = Env $ flip concatMap sts $ \st@Station {..} ->
    if stCapacity == 1
        then [st]
        else [ Station (stName ++ "sub" ++ show i) stFunc 1 | i <- [1 .. stCapacity] ]

intersectStations :: [(String, Time)] -> [(String, Time)] -> [(String, Time, Time)]
intersectStations xs ys = aux (sort' xs) (sort' ys)
    where
        sort' = sortBy (\a b -> compare (fst a) (fst b))
        aux [] _ = []
        aux _ [] = []
        aux (x@(n, t) : xs) (y@(n', t') : ys)
            | n < n' = aux xs (y:ys)
            | n == n' = (n, t, t') : aux xs ys
            | n > n' = aux (x:xs) ys
            