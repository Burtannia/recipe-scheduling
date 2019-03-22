module Recipe.Display
    ( toDot       
    ) where

import Recipe.Recipe
import Algebra.Graph
import qualified Data.GraphViz as G
import qualified Data.GraphViz.Attributes.Complete as G
import qualified Data.GraphViz.Types as G
import qualified Data.Text.Lazy as TL
import qualified Data.Text.Lazy.IO as TL

-- | Saves a recipe to a dot file with the given name.
-- This can then be used to generate an image using GraphViz.
toDot :: Recipe -> String -> IO ()
toDot (R g) fName = do
    let vs = map (\x -> (toString x, "")) (vertexList g)
        es = map (\(s,d) -> (toString s, toString d, "")) (edgeList g)
        dotGraph = G.graphElemsToDot dotParams vs es :: G.DotGraph String
        dotText = G.printDotGraph dotGraph :: TL.Text
    TL.writeFile (fName ++ ".dot") dotText

dotParams :: G.GraphvizParams String String String () String
dotParams = G.defaultParams

toString :: IxAction -> String
toString (i, a) = show i ++ ": " ++ show a