{-# LANGUAGE GeneralizedNewtypeDeriving #-}

{-|
This module defines the combinators
used to create recipes.
-}

module Recipe.Recipe (
    -- * Types
    IxAction, Recipe (..), Action (..),
    Recipe', STRecipe, Temp (..),
    Time (..), Condition (..),
    Measurement (..),

    -- * Combinators
    mkRecipe,
    ingredient, heat, heatAt, wait,
    mix, conditional, transaction,
    measure, using, separate, with,

    -- * Conditional Helper Functions
    forTime, toTemp, heatTo, heatFor,
    heatAtFor, waitFor,

    -- * Time Helper Functions
    minutes, hours,

    -- * Measurement Helper Functions
    grams, ml, count, getMeasure,

    -- * Multiple Output Helper Functions
    discardFirst, discardSecond
    ) where

import Algebra.Graph
import Control.Monad.State.Lazy

-- | An action paired with a unique label.
type IxAction = (Int, Action)

-- | A recipe is a wrapper around a graph of actions.
newtype Recipe = R (Graph IxAction)
    deriving Show

instance Eq Recipe where
    (R r) == (R r') = fmap snd r == fmap snd r'

-- | A recipe paired with the last action
-- to be added.
type Recipe' = (Recipe, IxAction)

-- | A recipe paired with the last action
-- to be added with state keeping track
-- of the unqiue label counter for actions.
type STRecipe = State Int Recipe'

-- Each node in the recipe graph
-- contains an action, not used directly.
-- Recipes are created via the functions below.
data Action = GetIngredient String
    | Heat
    | HeatAt Temp
    | Wait 
    | Mix 
    | Conditional Condition Action 
    | Transaction Action
    | Measure Measurement
    | Using [String] Action 
    | Separate String
    | With
    deriving (Show, Eq, Ord)

-- |'Temp' is a wrapper around an Int representing degrees centigrade.
newtype Temp = Temp Int
    deriving (Show, Eq, Ord, Num, Real, Enum, Integral)

instance Semigroup Temp where
    (<>) = (+)

instance Monoid Temp where
    mempty = 0

-- |'Time' is a wrapper around an Int representing seconds.
newtype Time = Time Int
    deriving (Eq, Ord, Num, Real, Enum, Integral)

-- |'Time' is printed in hms format.
instance Show Time where
    show (Time i) = let h = i `div` 3600
                        m = (i `mod` 3600) `div` 60
                        s = (i `mod` 3600) `mod` 60 in
                    show h ++ "h "
                    ++ show m ++ "m "
                    ++ show s ++ "s"

instance Semigroup Time where
    (<>) = (+)

instance Monoid Time where
    mempty = 0

-- |Conditions represent an event which you perform an action "until".
data Condition = CondTime Time -- ^ Until the given time has elapsed.
               | CondTemp Temp -- ^ Until the temperature has been reached.
               | Condition `AND` Condition -- ^ Conjunction of two conditions.
               | Condition `OR` Condition -- ^ Disjunction of two conditions.
               deriving (Show, Eq, Ord)

-- |Represents a measurement of something.
data Measurement = Count Int -- ^ Number of something e.g. 1 apple.
                 | Grams Int -- ^ Number of grams.
                 | Milliletres Int -- ^ Number of milliletres.
                 deriving (Eq)

-- |Returns the magnitude of a 'Measurement'.
getMeasure :: Measurement -> Int
getMeasure (Count i)       = i
getMeasure (Grams i)       = i
getMeasure (Milliletres i) = i

-- | > show ('Count' 1) = 1
-- > show ('Grams' 100) = 100g
-- > show ('Milliletres' 200) = 200ml
instance Show Measurement where
    show (Count i)       = show i
    show (Grams i)       = show i ++ "g"
    show (Milliletres i) = show i ++ "ml"

-- | Compares magnitude only, conversion between
-- units is still to be implemented.
instance Ord Measurement where
    compare a b = compare (getMeasure a) (getMeasure b)

-- | Evaluate the state starting at 0 and
-- generate a recipe.
mkRecipe :: STRecipe -> Recipe
mkRecipe sr = fst $ evalState sr 0

-- Combinator helper functions

insertAfter :: a -> a -> Graph a -> Graph a
insertAfter v last g = overlay g $ connect (vertex last) (vertex v)

insertHelper :: Action -> STRecipe -> STRecipe
insertHelper a sr = do
    (R r, last) <- sr
    i <- get
    put (i + 1)
    let v = (i, a)
        r' = insertAfter v last r
    return (R r', v)

wrapHelper :: (Action -> Action) -> STRecipe -> STRecipe
wrapHelper f sr = do
    (R r, last@(i,a)) <- sr
    let v = (i, f a)
    return (R (replaceVertex last v r), v)

-- Combinators

-- | An ingredient with a given name.
ingredient :: String -> STRecipe
ingredient name = do
    i <- get
    put (i + 1)
    let v = (i, GetIngredient name)
    return (R (vertex v), v)

-- | Heat dependencies, used in conjunction with 'conditional'.
heat :: STRecipe -> STRecipe
heat = insertHelper Heat

-- | Heat dependencies at a given temperature, used in conjunction with 'conditional'.
heatAt :: Temp -> STRecipe -> STRecipe
heatAt temp = insertHelper (HeatAt temp)

-- | Do nothing, used in conjunction with 'conditional'.
wait :: STRecipe -> STRecipe
wait = insertHelper (Wait)

-- | Mix dependencies together.
mix :: STRecipe -> STRecipe -> STRecipe
mix sr1 sr2 = do
    (R r1, last1) <- sr1
    (R r2, last2) <- sr2
    i <- get
    put (i + 1)
    let v = (i, Mix)
        r = overlay r1 $ overlay r2 $ edges [(last1, v), (last2, v)]
    return (R r, v)

-- | Serve dependencies with each other.
with :: STRecipe -> STRecipe -> STRecipe
with sr1 sr2 = do
    (R r1, last1) <- sr1
    (R r2, last2) <- sr2
    i <- get
    put (i + 1)
    let v = (i, With)
        r = overlay r1 $ overlay r2 $ edges [(last1, v), (last2, v)]
    return (R r, v)

-- | Wrap an action with a termination condition.
conditional :: Condition -> STRecipe -> STRecipe
conditional c = wrapHelper (Conditional c)

-- | Wrapping an action in a transaction means that all dependencies
-- must be completed around the same time and that the wrapped action
-- must be started within a small time of the dependencies being completed.
transaction :: STRecipe -> STRecipe
transaction = wrapHelper Transaction

-- | Measure the dependencies.
measure :: Measurement -> STRecipe -> STRecipe
measure m = insertHelper (Measure m)

-- | Perform the given action using a station with one of the given names.
using :: [String] -> STRecipe -> STRecipe
using sts = wrapHelper (Using sts)

-- | Used when a dependency produces multiple outputs
-- to label which output is being referred to.
separate :: String -> String -> STRecipe -> State Int (Recipe', Recipe')
separate s1 s2 sr = do
    (R r, last) <- sr
    i <- get
    let i' = i + 1
    put (i + 2)

    let v1 = (i, Separate s1)
        v2 = (i', Separate s2)
        r' = overlay r $ edges [(last, v1), (last, v2)]
        r1 = (R r', v1)
        r2 = (R r', v2)
        
    return (r1, r2)

-- Conditional helper functions

-- | Wrap previously added action in condition
-- of time.
forTime :: Time -> STRecipe -> STRecipe
forTime time = conditional (CondTime time)

-- | Wrap previously added action in condition
-- of temperature.
toTemp :: Temp -> STRecipe -> STRecipe
toTemp temp = conditional (CondTemp temp)

-- | Concatenation of 'heat' and 'toTemp'.
heatTo :: Temp -> STRecipe -> STRecipe
heatTo temp = toTemp temp . heat

-- | Concatenation of 'heat' and 'forTime'.
heatFor :: Time -> STRecipe -> STRecipe
heatFor time = forTime time . heat

-- | Concatenation of 'heatAt' and 'forTime'.
heatAtFor :: Temp -> Time -> STRecipe -> STRecipe
heatAtFor temp time = forTime time . heatAt temp

-- | Concatenation of 'wait' and 'forTime'.
waitFor :: Time -> STRecipe -> STRecipe
waitFor time = forTime time . wait

-- Time helper functions

-- | > minutes 5 = Time 300
minutes :: Int -> Time
minutes n = Time $ n * 60

-- | > hours 2 = Time 7200
hours :: Int -> Time
hours n = Time $ n * 60 * 60

-- Measurement helper functions

-- | > grams 100 = measure (Grams 100)
grams :: Int -> STRecipe -> STRecipe
grams n = measure (Grams n)

-- | > ml 200 = measure (Milliletres 200)
ml :: Int -> STRecipe -> STRecipe
ml n = measure (Milliletres n)

-- | > count 5 = measure (Count 5)
count :: Int -> STRecipe -> STRecipe
count n = measure (Count n)

-- Separate helper functions

-- | Discard the first output produced by
-- the previously added action and keep the second.
discardFirst :: State Int (Recipe', Recipe') -> STRecipe
discardFirst = fmap snd

-- | Discard the second output produced by
-- the previously added action and keep the first.
discardSecond :: State Int (Recipe', Recipe') -> STRecipe
discardSecond = fmap fst