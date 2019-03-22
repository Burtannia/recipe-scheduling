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