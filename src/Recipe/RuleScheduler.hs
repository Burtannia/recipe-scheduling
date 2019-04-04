{-# LANGUAGE RecordWildCards #-}

module Recipe.RuleScheduler where

import Recipe.Recipe
import Recipe.Helper
import Recipe.Kitchen
import Algebra.Graph
import Data.List ((\\), sortBy, minimumBy, maximumBy)

type ActionRule = [IxAction] -> Recipe -> Env -> IxAction

type StationRule = [(String, Time)] -> IxAction -> Recipe -> Schedule -> (String, Time)

type Schedule = [(String, [Task])]

data Task = Idle Float
          | Active IxAction Float
          deriving Show

shortestFirst :: ActionRule
shortestFirst = durationRule minimumBy

longestFirst :: ActionRule
longestFirst = durationRule maximumBy
    
durationRule :: (((IxAction, Time) -> (IxAction, Time) -> Ordering)
    -> [(IxAction, Time)] -> (IxAction, Time)) -> ActionRule
durationRule f as r env = fst $
    f (\(_, t) (_, t') -> compare t t') as'
    where
        as' = map (\a -> (a, averageTime $ validStations a r env)) as
        averageTime xs = (foldr (\(_, t) n -> n + t) 0 xs) `div` (fromIntegral $ length xs)

leastFlexible :: ActionRule
leastFlexible as r env = fst $
    minimumBy (\(_, n) (_, m) -> compare n m) as'
    where
        as' = map (\a -> (a, length $ validStations a r env)) as

fastestFirst :: StationRule
fastestFirst sts _ _ _ = minimumBy (\(_, t) (_, t') -> compare t t') sts

leastIdleReq :: StationRule
leastIdleReq sts a r sch = minimumBy (\(st, _) (st', _) ->
    compare (calcIdle a st sch r) (calcIdle a st' sch r)) sts

leastUsed :: StationRule
leastUsed sts _ _ sch = minimumBy (\(st, _) (st', _) ->
    compare (stationDuration st sch) (stationDuration st' sch)) sts

mkEmptySch :: Env -> Schedule
mkEmptySch (Env sts) = [(stName, []) | Station {..} <- sts]

ruleSchedule :: ActionRule -> StationRule -> Recipe -> Env -> Schedule
ruleSchedule aRule sRule fullR env = ruleSchedule' fullR (mkEmptySch env)
    where
        ruleSchedule' :: Recipe -> Schedule -> Schedule
        ruleSchedule' (R Empty) sch = sch
        ruleSchedule' r sch = ruleSchedule' (liftR' (removeVertex a) r) sch'
            where
                idleReq = calcIdle a st sch fullR
                as = filter (\(n, _) -> n == 0) $
                        map (\x -> (edgesTo x r, x)) (liftR vertexList r)
                a = aRule (map snd as) fullR env
                (st, dur) = sRule (validStations a fullR env) a fullR sch
                task = Active a (fromIntegral dur)
                ts = if idleReq > 0 then [Idle idleReq, task] else [task]
                sch' = addTasks ts st sch

edgesTo :: IxAction -> Recipe -> Int
edgesTo a (R g) = foldr (\(_, a') n ->
    if a == a' then n + 1 else n) 0 (edgeList g)

-- | Append a list of tasks to a station in the given schedule.
addTasks :: [Task] -> String -> Schedule -> Schedule
addTasks toAdds stName sch = go sch
    where
        go [] = []
        go (x@(st, ts) : xs)
            | st == stName = (st, ts ++ toAdds) : xs
            | otherwise = x : go xs

-- | Calculate the idle time required to schedule
-- the given action on the given station in the
-- given schedule.
calcIdle :: IxAction -> String -> Schedule -> Recipe -> Float
calcIdle a stName sch r = if startTime < depsEnd
                            then depsEnd - startTime
                            else 0
    where
        ds = deps a r
        depsEnd = if ds == []
            then 0
            else maximum $ map (\a -> endOfTask a sch) ds
        startTime = stationDuration stName sch

-- | The end time of the final task of a station in
-- the given schedule.
stationDuration :: String -> Schedule -> Float
stationDuration st sch = case sch' of
    [] -> 0
    ((_, ts) : _) -> foldr (\t n -> n + getDur t) 0 ts
    where
        sch' = filter (\(st', _) -> st == st') sch

-- | Get the duration of a task.
getDur :: Task -> Float
getDur (Idle f) = f
getDur (Active _ f) = f

-- | Get the end time of a task in the given schedule.
endOfTask :: IxAction -> Schedule -> Float
endOfTask a sch = foldr (\(st, ts) n -> max n $ sumTasks ts) 0 sch
    where
        sumTasks [] = 0
        sumTasks (Idle f : ts) = f + sumTasks ts
        sumTasks (Active a' f : ts)
            | a == a' = f
            | otherwise = f + sumTasks ts