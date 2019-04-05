{-# LANGUAGE RecordWildCards #-}

module Recipe.Kitchen where

import Recipe.Recipe
import Recipe.Helper
import Algebra.Graph
import Data.Maybe (isJust, catMaybes)
import Data.List (sortBy, maximumBy)
import Data.Ord (comparing)

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

validStations' :: IxAction -> Recipe -> Env -> [(Station, Time)]
validStations' a r (Env sts) = catMaybes $
    flip map sts $ \st@Station {..} ->
        fmap (\time -> (st, time)) $
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
        sort' = sortBy (comparing fst)
        aux [] _ = []
        aux _ [] = []
        aux (x@(n, t) : xs) (y@(n', t') : ys)
            | n < n' = aux xs (y:ys)
            | n == n' = (n, t, t') : aux xs ys
            | n > n' = aux (x:xs) ys
            
infeasible :: Recipe -> Env -> Maybe [IxAction]
infeasible r env = foldr aux Nothing as
    where
        as = liftR vertexList r
        aux a mx = joinRes mx (hasStations a)
        hasStations a = case validStations a r env of
            [] -> Just [a]
            _  -> Nothing
        joinRes mx Nothing = mx
        joinRes Nothing my = my
        joinRes (Just xs) (Just ys) = Just $ xs ++ ys

maxEnd :: Recipe -> Env -> Time
maxEnd r env = sum ts
    where
        as = liftR vertexList r
        ts = map (\a ->
            snd $ maximumBy (comparing snd) $ validStations a r env) as


maxDur :: Recipe -> Env -> Time
maxDur r env = maximum ts
    where
        as = liftR vertexList r
        ts = map (\a ->
            snd $ maximumBy (comparing snd) $ validStations a r env) as 