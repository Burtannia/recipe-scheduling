module Recipe.Scheduler where

import Recipe.Recipe
import Recipe.Helper
import Recipe.Kitchen
import LP.Lang
import Algebra.Graph
import Data.List ((\\))
import Data.Maybe (catMaybes)

writeSchedule :: String -> Recipe -> Env -> IO ()
writeSchedule fName r e = do
    let fPath = fName ++ ".lp"
        env@(Env sts) = fromCapacity e
        objf = ObjF ObjMin [var1 "Emax"]
        as = liftR vertexList r
        ixs = map fst as

    -- Constraints

    -- (1) Ei >= E_ij
    let c1 = [ [var1 "Emax"] `geq` [var1 $ "E_" ++ show i ++ '_' : j]
                | a@(i, _) <- as, (j, _) <- validStations a r env]
    
    let czero = [ [var1 $ "E_" ++ show i ++ '_' : j] `leq` [var (bigM, "X_" ++ show i ++ '_' : j)]
                | a@(i, _) <- as, (j, _) <- validStations a r env]

    -- (2) E_ij >= D_ij * X_ij
    let c2 = [ [var1 $ "E_" ++ show i ++ '_' : j] `geq` [var (fromIntegral dij, "X_" ++ show i ++ '_' : j)]
                | a@(i, _) <- as, (j, dij) <- validStations a r env]

    -- (3) E_ij >= E_kl + (D_ij * X_ij)
    let c3 = [  [ var1 $ "E_" ++ show i ++ '_' : j ]
                `geq`
                [ var1 $ "E_" ++ show k ++ '_' : l
                , var (fromIntegral dij, "X_" ++ show i ++ '_' : j) ]
                | ai@(i, _) <- as
                , ak@(k, _) <- deps ai r
                , (j, dij) <- validStations ai r env
                , (l, _) <- validStations ak r env ]

    -- (4) SUM X_ij = 1
    let c4 = [ [var1 $ "X_" ++ show i ++ '_' : j | (j, _) <- validStations a r env]
                `eql` [constant 1] | a@(i, _) <- as ]
        c4' = map (\(c@(C t xs ys), n) -> if length xs == 1 then (C t ((var1 $ "dummy_" ++ show n) : xs) ys, Just n) else (c, Nothing)) (zip c4 [1..])
        c4'' = map fst c4'
        ds = map (\n -> var1 $ "dummy_" ++ show n) $ catMaybes $ map snd c4'
        dConstraint = C Eql ds [constant 0]

    -- (5) E_ij - E_kj >= D_ij - (M * Y_ijk)
    let c5 = [ [ var1 $ "E_" ++ show i ++ '_' : j
               , varNeg1 $ "E_" ++ show k ++ '_' : j ]
               `geq`
               [ constant $ fromIntegral dij -- maybe add multiplier with X or Y here
               , var (negate bigM, "Y_" ++ show i ++ ('_' : j) ++ ('_' : show k)) ]
               | ai@(i, _) <- as
               , ak@(k, _) <- as \\ [ai]
               , (j, dij, _) <- intersectStations (validStations ai r env)
                                                  (validStations ak r env)
                                                  ]

    -- (6) E_ij - E_kj >= D_ij - (M * Y_ijk)
    let c6 = [ [ var1 $ "E_" ++ show k ++ '_' : j
               , varNeg1 $ "E_" ++ show i ++ '_' : j ]
               `geq`
               [ constant $ fromIntegral dkj -- maybe add multiplier with X or Y here
               , constant $ negate bigM
               , var (bigM, "Y_" ++ show i ++ ('_' : j) ++ ('_' : show k)) ]
               | ai@(i, _) <- as
               , ak@(k, _) <- as \\ [ai]
               , (j, _, dkj) <- intersectStations (validStations ai r env)
                                                  (validStations ak r env)
                                                  ]

    -- (7) bin X_ij
    let c7 = BinC
            [ (1, "X_" ++ show i ++ '_' : j) | a@(i, _) <- as
                                             , (j, _) <- validStations a r env
                                             ]

    -- (8) bin Y_ijk
    let c8 = BinC
            [ (1, "Y_" ++ show i ++ ('_' : j) ++ ('_' : show k))
                | ai@(i, _) <- as
                , ak@(k, _) <- as
                , (j, _, dkj) <- intersectStations (validStations ai r env)
                                                   (validStations ak r env)
                                                   ]

    let constraints = czero ++ c1 ++ c2 ++ c3 ++ c4'' ++ (dConstraint : c5) ++ c6 ++ [c7] ++ [c8]
        model = Model objf constraints

    -- currently ignoring transactions

    writeLP fPath model