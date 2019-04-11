{-# LANGUAGE RecordWildCards #-}

{-|
This module defines the environment in
which recipes are scheduled.
-}

module Recipe.Kitchen (
    -- * Types
    Station (..), Env (..),

    -- * Scheduling Helper Functions
    fromCapacity, validStations, validStations',
    isValidSt, intersectStations, infeasible,
    maxEnd, maxDur ) where

import Recipe.Recipe
import Recipe.Helper
import Algebra.Graph
import Data.Maybe (isJust, catMaybes)
import Data.List (sortBy, maximumBy)
import Data.Ord (comparing)

-- | Represents a station (such as an oven)
-- within the environment.
data Station = Station
    { stName :: String -- ^ Unique name
    , stFunc :: Action -> [Action] -> Maybe Time -- ^ Given an action and its dependencies returns the time
                                                    -- take for the station to perform that action.
                                                    -- Returns Nothing if the station cannot perform the given action.
    , stCapacity :: Int -- ^ Number of concurrent actions the station can perform.
    }

instance Eq Station where
    s1 == s2 = (stName s1) == (stName s2)

instance Ord Station where
    compare s1 s2 = compare (stName s1) (stName s2)

-- | An environment is simply a wrapper
-- around a list of station in the environment.
data Env = Env [Station]

-- | Given an action from a given recipe and an environment,
-- produces a list of station names paired with the time it
-- would take each station to perform the recipe.
validStations :: IxAction -> Recipe -> Env -> [(String, Time)]
validStations a r (Env sts) = catMaybes $
    flip map sts $ \Station {..} ->
        fmap (\time -> (stName, time)) $
            stFunc (snd a) (map snd $ deps a r)

-- | Same as 'validStations' but pairs the time with the
-- station, not just the station name.
validStations' :: IxAction -> Recipe -> Env -> [(Station, Time)]
validStations' a r (Env sts) = catMaybes $
    flip map sts $ \st@Station {..} ->
        fmap (\time -> (st, time)) $
            stFunc (snd a) (map snd $ deps a r)

-- | Given an action and its dependencies returns True
-- if the given station can perform the action.
isValidSt :: Action -> [Action] -> Station -> Bool
isValidSt a ds Station {..} = isJust $ stFunc a ds

-- | Splits a station down into one substation for
-- each concurrent action it is capable of performing.
fromCapacity :: Env -> Env
fromCapacity (Env sts) = Env $ flip concatMap sts $ \st@Station {..} ->
    if stCapacity == 1
        then [st]
        else [ Station (stName ++ "sub" ++ show i) stFunc 1 | i <- [1 .. stCapacity] ]

-- | Creates the intersection of two lists of stations
-- produced by 'validStations'. The intersection only
-- considers the station name hence both times are
-- returned as they may be different.
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
            
-- | Returns a list of actions from the given recipe
-- that cannot be performed by any station in the
-- given environment.
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

-- | Maximum end time of the recipe if
-- every action was performed sequentially
-- using the slowest station for each action.
maxEnd :: Recipe -> Env -> Time
maxEnd r env = sum ts
    where
        as = liftR vertexList r
        ts = map (\a ->
            snd $ maximumBy (comparing snd) $ validStations a r env) as

-- | Maximum duration of any single action
-- in the given recipe.
maxDur :: Recipe -> Env -> Time
maxDur r env = maximum ts
    where
        as = liftR vertexList r
        ts = map (\a ->
            snd $ maximumBy (comparing snd) $ validStations a r env) as 