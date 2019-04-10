{-|
This module contains various helper
functions for use with recipes.
-}

module Recipe.Helper (
    -- * General
    ppList, deps, reverseR,
    
    -- * Lifting function to work on recipes
    liftR, liftR', liftR2',
    ) where

import Recipe.Recipe
import Algebra.Graph
import Data.Tuple (swap)
import Data.Maybe (fromJust)

-- | Prints a list with every element
-- on a new line.
ppList :: Show a => [a] -> IO ()
ppList [] = return ()
ppList (x:xs) = do
    print x
    ppList xs

-- | Lift a function on graphs of actions to recipes.
liftR' :: (Graph IxAction -> Graph IxAction) -> Recipe -> Recipe
liftR' f (R r) = R (f r)

-- | Lift a function over two graphs of actions to
-- a function over two recipes.
liftR2' :: (Graph IxAction -> Graph IxAction
    -> Graph IxAction) -> Recipe -> Recipe -> Recipe
liftR2' f (R r1) (R r2) = R (f r1 r2)

-- | Lift a polymorphic function on graphs
-- of actions to a function on recipes.
liftR :: (Graph IxAction -> a) -> Recipe -> a
liftR f (R r) = f r

-- | Get the dependencies of a given action
-- in the given recipe.
deps :: IxAction -> Recipe -> [IxAction]
deps a r = fromJust $
    lookup a (liftR adjacencyList $ reverseR r)

-- | Reverse all the edges in the given
-- recipe graph.
reverseR :: Recipe -> Recipe
reverseR = liftR' $ edges . map swap . edgeList