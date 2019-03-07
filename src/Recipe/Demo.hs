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
    $ forTime 300 $ wait
    $ mix teabag
    $ toTemp 100 (heat water)

cupOfTea' :: Recipe
cupOfTea' = mkRecipe
    $ mix (discardFirst
            $ separate "teabag" "tea"
            $ forTime 300 $ wait
            $ mix teabag
            $ toTemp 100 (heat water)) milk
