module Recipe.Demo where

import Recipe.Recipe
import Recipe.Kitchen
import Recipe.RuleScheduler

-- Cup of tea

water, teabag, milk :: STRecipe
water = ingredient "water"
teabag = ingredient "teabag"
milk = ingredient "milk"

cupOfTea :: Recipe
cupOfTea = mkRecipe
    $ mix milk
    $ discardFirst
    $ separate "teabag" "tea"
    $ waitFor (minutes 5)
    $ mix teabag
    $ heatTo 100 water

cupOfTea' :: Recipe
cupOfTea' = mkRecipe
    $ mix (discardFirst
            $ separate "teabag" "tea"
            $ waitFor (minutes 5)
            $ mix teabag
            $ heatTo 100 water) milk

-- Rice

rice :: STRecipe
rice = ingredient "rice"

boiledRice :: STRecipe
boiledRice = transaction
    $ discardFirst
    $ separate "water" "rice"
    $ heatFor (minutes 10)
    $ mix (grams 60 rice) (ml 220 water)

-- Chicken Jalfrezi

tinnedTomatoes, redPepper, onion, garlic, oil :: STRecipe
tinnedTomatoes = ingredient "tinned tomatoes"
redPepper = ingredient "red pepper"
onion = ingredient "onion"
garlic = ingredient "garlic"
oil = ingredient "olive oil"

cumin, coriander, turmeric, paprika, garamMasala :: STRecipe
cumin = ingredient "cumin"
coriander = ingredient "coriander"
turmeric = ingredient "turmeric"
paprika = ingredient "paprika"
garamMasala = ingredient "garam masala"

dicedChicken :: STRecipe
dicedChicken = ingredient "dicedChicken"

spiceMix :: STRecipe
spiceMix = mix cumin
    $ mix coriander
    $ mix turmeric paprika

marinate :: Time -> STRecipe -> STRecipe -> STRecipe
marinate time r mar = waitFor time
    $ mix r mar

spicedChicken :: STRecipe
spicedChicken = heatTo 75
    $ marinate 10 dicedChicken spiceMix

jalfreziSauce :: STRecipe
jalfreziSauce = heatFor (minutes 10)
    $ mix tinnedTomatoes
    $ heatFor (minutes 5)
    $ mix redPepper
    $ heatFor (minutes 5)
    $ mix spiceMix
    $ mix onion
    $ mix garlic oil

jalfreziWithRice :: Recipe
jalfreziWithRice = mkRecipe
    $ transaction
    $ with boiledRice
    $ heatFor (minutes 5)
    $ mix garamMasala
    $ heatFor (minutes 10)
    $ mix spicedChicken jalfreziSauce

-- Roast Chicken

wholeChicken, salt, pepper, flour, chickenStock :: STRecipe
wholeChicken = ingredient "whole chicken"
salt = ingredient "salt"
pepper = ingredient "pepper"
flour = ingredient "flour"
chickenStock = ingredient "chicken stock"

roastChicken :: STRecipe
roastChicken = heatAtFor 160 (minutes 25)
    $ heatAtFor 180 (minutes 60)
    $ mix wholeChicken
    $ mix salt pepper
    
makeGravy :: STRecipe -> STRecipe
makeGravy juices = heatFor (minutes 10)
    $ mix chickenStock
    $ mix flour
    $ heatFor (minutes 1) juices

separateDoWith :: String -> String -> (STRecipe -> STRecipe)
    -> (STRecipe -> STRecipe) -> STRecipe -> STRecipe
separateDoWith s1 s2 r1 r2 r = do
    (x1, x2) <- separate s1 s2 r
    with (r1 $ return x1) (r2 $ return x2)

doNothing :: STRecipe -> STRecipe
doNothing = id

chickenWithGravy :: Recipe
chickenWithGravy = mkRecipe $
    separateDoWith "chicken" "juices" doNothing makeGravy roastChicken

-- Env

testEnv :: Env
testEnv = Env [kettle, chef, worktop, hob, oven]

kettle :: Station
kettle = Station "kettle" f 1
    where
        f (Conditional (CondTemp 100) Heat) [GetIngredient "water"] = Just 120
        f (Transaction a) ds = f a ds
        f (Using xs a) ds = if "kettle" `elem` xs then f a ds else Nothing
        f _ _ = Nothing

chef :: Station
chef = Station "chef" f 1
    where
        f a ds = case a of
            GetIngredient _ -> Just 10
            Mix -> Just 7
            Transaction a -> f a ds
            Separate _ -> Just 5
            With -> Just 5
            Measure _ -> Just 20
            Using xs a -> if "chef" `elem` xs then f a ds else Nothing
            _ -> Nothing

worktop :: Station
worktop = Station "worktop" f 1
    where
        f a ds = case a of
            Wait -> Just 0
            Conditional (CondTime t) a -> f a ds >>= const (Just t)
            Transaction a -> f a ds
            Using xs a -> if "worktop" `elem` xs then f a ds else Nothing
            _ -> Nothing

hob :: Station
hob = Station "hob" f 2
    where
        f a ds = case a of
            Heat -> Just 0
            Conditional (CondTime t) a -> f a ds >>= const (Just t)
            Conditional (CondTemp t) a -> f a ds >>= return . (+) (fromIntegral t * 4)
            Transaction a -> f a ds
            Using xs a -> if "hob" `elem` xs then f a ds else Nothing
            _ -> Nothing

oven :: Station
oven = Station "oven" f 2
    where
        f a ds = case a of
            Heat -> Just 0
            HeatAt t -> if t <= 220 && t >= 160 then Just 0 else Nothing
            Conditional (CondTime t) a -> f a ds >>= const (Just t)
            Conditional (CondTemp t) a -> f a ds >>= return . (+) (fromIntegral t * 5)
            Transaction a -> f a ds
            Using xs a -> if "oven" `elem` xs then f a ds else Nothing
            _ -> Nothing

-- Scheduling

aRules :: [(String, ActionRule)]
aRules = [ ("Short", shortestFirst)
         , ("Long", longestFirst)
         , ("Flex", leastFlexible)
         ]

stRules :: [(String, StationRule)]
stRules = [ ("Fast", fastestFirst)
          , ("Idle", leastIdleReq)
          , ("Used", leastUsed)
          ]

heuristics :: [(String, ActionRule, StationRule)]
heuristics = [(an ++ sn, ar, sr) | (sn, sr) <- stRules, (an, ar) <- aRules]

testRecipes :: [(String, Recipe)]
testRecipes = [ ("Tea", cupOfTea)
              , ("Jalfrezi", jalfreziWithRice)
              , ("Gravy", chickenWithGravy)
              ]

testResults :: [(String, String, Float)]
testResults = flip map tests $ 
    \((rName, r), (hName, aRule, stRule)) ->
        (rName, hName, scheduleLength $ ruleSchedule aRule stRule r testEnv)
    where
        tests = [(r, h) | r <- testRecipes, h <- heuristics]