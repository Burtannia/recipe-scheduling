module DAG (
    empty,
    addNode,
    addEdge,
    linkNode,
    join,
    numEdges,
    numNodes
    ) where

import Data.Sequence hiding (empty)
import Prelude hiding (length)
import qualified Data.List as L (length)

data DAG a = DAG (Seq (a, [Int]))
    deriving Show
         
instance Ord a => Eq (DAG a) where
    (DAG xs) == (DAG ys) =
        let xs' = unstableSort xs
            ys' = unstableSort ys
         in xs' == ys'

instance Ord a => Ord (DAG a) where
    compare (DAG xs) (DAG ys) =
        let xs' = unstableSort xs
            ys' = unstableSort ys
         in compare xs' ys'

-- |The empty graph.
empty :: DAG a
empty = DAG Empty

-- |Add a node to the graph.
addNode :: a -> DAG a -> DAG a
addNode a (DAG as) = DAG $ as |> (a, [])

-- |Add an edge between two indexed nodes.
addEdge :: Int -> Int -> DAG a -> DAG a
addEdge x y (DAG as) = DAG $
    adjust' (appSnd (y:)) x as

-- |Add a new node to the graph and
-- an edge from the previous node to
-- to the new node.
linkNode :: a -> DAG a -> DAG a
linkNode a d =
    let d'@(DAG as) = addNode a d
        l = length as
     in addEdge (l-2) (l-1) d'

join :: DAG a -> DAG a -> DAG a
join (DAG xs) (DAG ys) =
    let l = length as
        ys' = fmap (\(n,as) -> (n, map (+l) as)) ys
     in DAG $ xs >< ys'

joinWithLink :: DAG a -> DAG a -> a -> DAG a
joinWithLink d1 d2 a =
    addEdge (n1 - 1) (n1 + n2) $
    addEdge (n1 + n2 - 1) (n1 + n2) $
    addNode a $ join d1 d2
    where
        n1 = numNodes d1
        n2 = numNodes d2

appSnd :: (b -> c) -> (a,b) -> (a,c)
appSnd f (a,b) = (a, f b)

numNodes :: DAG a -> Int
numNodes (DAG as) = length as

numEdges :: DAG a -> Int
numEdges (DAG as) = foldr (\(_,xs) n -> L.length xs + n) 0 as