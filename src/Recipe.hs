module Recipe where

import DAG

type Recipe = DAG Action

data Action = GetIngredient String
    | Heat
    | HeatAt Int
    | Wait
    | Combine String
    | Conditional Action Condition
    | Transaction Action
    | Measure Measurement
    | Optional String Action
    | Using Action [String]
    deriving (Show, Eq, Ord)

ingredient :: String -> Recipe
ingredient s = addNode (GetIngredient s) empty

heat :: Recipe -> Recipe
heat = linkNode Heat

heatAt :: Int -> Recipe -> Recipe
heatAt t = linkNode (HeatAt t)

wait :: Recipe -> Recipe
wait = linkNode Wait

combine :: String -> Recipe -> Recipe -> Recipe
combine s r1 r2 = joinWithLink (Combine s) r1 r2