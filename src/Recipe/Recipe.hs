{-# LANGUAGE GeneralizedNewtypeDeriving #-}

module Recipe.Recipe where

import Algebra.Graph
import Control.Monad.State.Lazy

type IxAction = (Int, Action)

newtype Recipe = R (Graph IxAction)
    deriving Show

instance Eq Recipe where
    (R r) == (R r') = fmap snd r == fmap snd r'

type Recipe' = (Recipe, IxAction)

type STRecipe = State Int Recipe'

data Action = GetIngredient String
    | Heat
    | HeatAt Temp
    | Wait
    | Mix
    | Conditional Condition Action 
    | Transaction Action
    | Measure Measurement
    -- | Optional String Action
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

mkRecipe :: STRecipe -> Recipe
mkRecipe sr = fst $ evalState sr 0

insertAfter :: a -> a -> Graph a -> Graph a
insertAfter v last g = overlay g $ connect (vertex last) (vertex v)

insertHelper :: Action -> STRecipe -> STRecipe
insertHelper a sr = do
    (R r, last) <- sr
    i <- get
    put (i + 1)
    let v = (i, a)
        r' = insertAfter v last r
    return $! (R r', v)

wrapHelper :: (Action -> Action) -> STRecipe -> STRecipe
wrapHelper f sr = do
    (R r, last@(i,a)) <- sr
    let v = (i, f a)
    return $! (R (replaceVertex last v r), v)

ingredient :: String -> STRecipe
ingredient name = do
    i <- get
    put (i + 1)
    let v = (i, GetIngredient name)
    return $! (R (vertex v), v)

heat :: STRecipe -> STRecipe
heat = insertHelper Heat

heatAt :: Temp -> STRecipe -> STRecipe
heatAt temp = insertHelper (HeatAt temp)

wait :: STRecipe -> STRecipe
wait = insertHelper (Wait)

mix :: STRecipe -> STRecipe -> STRecipe
mix sr1 sr2 = do
    (R r1, last1) <- sr1
    (R r2, last2) <- sr2
    i <- get
    put (i + 1)
    let v = (i, Mix)
        r = overlay r1 $ overlay r2 $ edges [(last1, v), (last2, v)]
    return $! (R r, v)

with :: STRecipe -> STRecipe -> STRecipe
with sr1 sr2 = do
    (R r1, last1) <- sr1
    (R r2, last2) <- sr2
    i <- get
    put (i + 1)
    let v = (i, With)
        r = overlay r1 $ overlay r2 $ edges [(last1, v), (last2, v)]
    return $! (R r, v)

conditional :: Condition -> STRecipe -> STRecipe
conditional c = wrapHelper (Conditional c)

transaction :: STRecipe -> STRecipe
transaction = wrapHelper Transaction

measure :: Measurement -> STRecipe -> STRecipe
measure m = insertHelper (Measure m)

-- optional :: String -> STRecipe -> STRecipe
-- optional lbl = wrapHelper (Optional lbl)

using :: [String] -> STRecipe -> STRecipe
using sts = wrapHelper (Using sts)

separate :: String -> String -> STRecipe -> State Int (Recipe', Recipe')
separate s1 s2 sr = do
    r1 <- insertHelper (Separate s1) sr
    r2 <- insertHelper (Separate s2) sr
    return $! (r1, r2) 

forTime :: Time -> STRecipe -> STRecipe
forTime time = conditional (CondTime time)

toTemp :: Temp -> STRecipe -> STRecipe
toTemp temp = conditional (CondTemp temp)

heatTo :: Temp -> STRecipe -> STRecipe
heatTo temp = toTemp temp . heat

heatFor :: Time -> STRecipe -> STRecipe
heatFor time = forTime time . heat

waitFor :: Time -> STRecipe -> STRecipe
waitFor time = forTime time . wait

minutes :: Int -> Time
minutes n = Time $ n * 60

grams :: Int -> STRecipe -> STRecipe
grams n = measure (Grams n)

ml :: Int -> STRecipe -> STRecipe
ml n = measure (Milliletres n)

count :: Int -> Measurement
count = Count

discardFirst :: State Int (Recipe', Recipe') -> STRecipe
discardFirst = fmap snd

discardSecond :: State Int (Recipe', Recipe') -> STRecipe
discardSecond = fmap fst