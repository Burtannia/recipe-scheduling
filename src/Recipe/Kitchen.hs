module Recipe.Kitchen where

import Recipe.Recipe

data Station = Station
    { stName :: String
    , stFunc :: Action -> Bool
    }

data Env = Env
    { eStations :: [Station]
    , eOpts :: [(String, Bool)]
    }