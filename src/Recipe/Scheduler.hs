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

    -- (1) Emax >= E_i
    let c1 = [ [var1 "Emax"] `geq` [var1 $ "E_" ++ show i] | i <- ixs ]

    -- (2) Ei >= E_ij
    let c2 = [ [var1 $ "E_" ++ show i] `geq` [var1 $ "E_" ++ show i ++ '_' : j]
                | a@(i, _) <- as, (j, _) <- validStations a r env]
    
    -- (3) E_ij >= D_ij * X_ij
    let c3 = [ [var1 $ "E_" ++ show i ++ '_' : j] `geq` [var (fromIntegral dij, "X_" ++ show i ++ '_' : j)]
                | a@(i, _) <- as, (j, dij) <- validStations a r env]

    -- (4) E_ij >= E_kl + (D_ij * X_ij)
    let c4 = [  [ var1 $ "E_" ++ show i ++ '_' : j ]
                `geq`
                [ var1 $ "E_" ++ show k ++ '_' : l
                , var (fromIntegral dij, "X_" ++ show i ++ '_' : j) ]
                | ai@(i, _) <- as
                , ak@(k, _) <- deps ai r
                , (j, dij) <- validStations ai r env
                , (l, _) <- validStations ak r env ]

    -- (5) SUM X_ij = 1
    let c5 = [ [var1 $ "X_" ++ show i ++ '_' : j | (j, _) <- validStations a r env]
                `eql` [constant 1] | a@(i, _) <- as ]
        c5' = map (\(c@(C t xs ys), n) -> if length xs == 1 then (C t ((var1 $ "dummy_" ++ show n) : xs) ys, Just n) else (c, Nothing)) (zip c5 [1..])
        c5'' = map fst c5'
        ds = map (\n -> var1 $ "dummy_" ++ show n) $ catMaybes $ map snd c5'
        dConstraint = C Eql ds [constant 0]

    -- (6) E_ij - E_kj >= D_ij - (M * Y_ijk)
    let c6 = [ [ var1 $ "E_" ++ show i ++ '_' : j
               , varNeg1 $ "E_" ++ show k ++ '_' : j ]
               `geq`
               [ constant $ fromIntegral dij
               , var (negate bigM, "Y_" ++ show i ++ ('_' : j) ++ ('_' : show k)) ]
               | ai@(i, _) <- as
               , ak@(k, _) <- as \\ [ai]
               , (j, dij, _) <- intersectStations (validStations ai r env)
                                                  (validStations ak r env)
                                                  ]

    -- (7) E_ij - E_kj >= D_ij - (M * Y_ijk)
    let c7 = [ [ var1 $ "E_" ++ show k ++ '_' : j
               , varNeg1 $ "E_" ++ show i ++ '_' : j ]
               `geq`
               [ constant $ fromIntegral dkj
               , constant $ negate bigM
               , var (bigM, "Y_" ++ show i ++ ('_' : j) ++ ('_' : show k)) ]
               | ai@(i, _) <- as
               , ak@(k, _) <- as \\ [ai]
               , (j, _, dkj) <- intersectStations (validStations ai r env)
                                                  (validStations ak r env)
                                                  ]

    -- (8) bin X_ij
    let c8 = BinC
            [ (1, "X_" ++ show i ++ '_' : j) | a@(i, _) <- as
                                             , (j, _) <- validStations a r env
                                             ]

    -- (9) bin Y_ijk
    let c9 = BinC
            [ (1, "Y_" ++ show i ++ ('_' : j) ++ ('_' : show k))
                | ai@(i, _) <- as
                , ak@(k, _) <- as
                , (j, _, dkj) <- intersectStations (validStations ai r env)
                                                   (validStations ak r env)
                                                   ]
    
    let constraints = c1 ++ c2 ++ c3 ++ c4 ++ c5'' ++ (dConstraint : c6) ++ c7 ++ [c8] ++ [c9]
        model = Model objf constraints

    -- currently ignoring transactions

    writeLP fPath model