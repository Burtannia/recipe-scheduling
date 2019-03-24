module Recipe.Demo where

import Recipe.Recipe
import Recipe.Kitchen

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

rice :: STRecipe
rice = ingredient "rice"

boiledRice :: STRecipe
boiledRice = transaction
    $ discardFirst
    $ separate "water" "rice"
    $ heatFor (minutes 10)
    $ mix (grams 60 rice) (ml 220 water)

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

chicken :: STRecipe
chicken = ingredient "chicken"

spiceMix :: STRecipe
spiceMix = mix cumin
    $ mix coriander
    $ mix turmeric paprika

marinate :: Time -> STRecipe -> STRecipe -> STRecipe
marinate time r mar = waitFor time
    $ mix r mar

spicedChicken :: STRecipe
spicedChicken = heatTo 75
    $ marinate 10 chicken spiceMix

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
    $ mix spicedChicken jalfreziSauce

testEnv :: Env
testEnv = Env [kettle, chef, worktop, hob]

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