module Recipe.RuleScheduler where

import Recipe.Recipe
import Recipe.Helper
import Recipe.Kitchen

type ActionRule = [IxAction] -> Recipe -> Env -> IxAction

type StationRule = [Station] -> IxAction -> Recipe -> Station

type Schedule = [(StName, [Task])]

data Task = Idle Float
          | Active IxAction Float

--ruleSchedule :: Rule -> Recipe -> Env -> Schedule
-- feasibility check? all actions can be done by at least 1 station
-- list of actions to schedule
-- filter via action rule
-- get list of valid stations
-- filter via station rule
-- if idle time needed add idle time to station
-- add action to station
-- remove action from recipe
-- if all actions scheduled, end
    -- else repeat