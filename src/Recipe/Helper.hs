module Recipe.Helper where

import Recipe.Recipe
import Algebra.Graph
import Data.Tuple (swap)
import Data.Maybe (fromJust)

ppList :: Show a => [a] -> IO ()
ppList [] = return ()
ppList (x:xs) = do
    print x
    ppList xs

liftR' :: (Graph IxAction -> Graph IxAction) -> Recipe -> Recipe
liftR' f (R r) = R (f r)

liftR2' :: (Graph IxAction -> Graph IxAction
    -> Graph IxAction) -> Recipe -> Recipe -> Recipe
liftR2' f (R r1) (R r2) = R (f r1 r2)

liftR :: (Graph IxAction -> a) -> Recipe -> a
liftR f (R r) = f r

deps :: IxAction -> Recipe -> [IxAction]
deps a r = fromJust $
    lookup a (liftR adjacencyList $ reverseR r)

reverseR :: Recipe -> Recipe
reverseR = liftR' $ edges . map swap . edgeList