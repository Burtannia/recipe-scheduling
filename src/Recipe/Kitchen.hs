module Recipe.Kitchen where

import Recipe.Recipe

type Capacity = Int

data Station = Station
    { stName :: String
    , stFunc :: Action -> Maybe (Time, Capacity)
    , stCapacity :: Capacity
    }

data Env = Env
    { eStations :: [Station]
    }

