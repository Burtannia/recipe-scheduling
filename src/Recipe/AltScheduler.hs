module Recipe.AltScheduler where

import Recipe.Recipe
import Recipe.Helper
import Recipe.Kitchen
import LP.Lang
import Algebra.Graph
import Data.Maybe (catMaybes)

writeSchedule' :: String -> Recipe -> Env -> IO ()
writeSchedule' fName r e = do
    let fPath = fName ++ ".lp"
        env@(Env sts) = fromCapacity e
        objf = ObjF ObjMin [var1 "Emax"]
        as = liftR vertexList r
        ixs = map fst as
        bigM = fromIntegral $ maxDur r env + maxEnd r env

    -- Constraints

    let sumdx = \a@(i,_) -> [ var (fromIntegral dij, "X_" ++ show i ++ '_' : j) | (j, dij) <- validStations a r env ]

    -- (1) Ei >= E_i
    let c1 = [ [var1 "Emax"] `geq` [var1 $ "E_" ++ show i] | a@(i, _) <- as ]

    -- (2) E_i >= SUM (D_ij * X_ij)
    let c2 = [ [ var1 $ "E_" ++ show i ] `geq` (sumdx a) | a@(i, _) <- as ]

    -- (3) E_i >= E_k + SUM (D_ij * X_ij)
    let c3 = [ [ var1 $ "E_" ++ show i]
                `geq`
                ((var1 $ "E_" ++ show k) : sumdx ai)
                | ai@(i, _) <- as
                , ak@(k, _) <- deps ai r ]

    -- (4) SUM X_ij = 1
    let c4 = [ [var1 $ "X_" ++ show i ++ '_' : j | (j, _) <- validStations a r env]
                `eql` [constant 1] | a@(i, _) <- as ]
        c4' = map (\(c@(C t xs ys), n) -> if length xs == 1 then (C t ((var1 $ "dummy_" ++ show n) : xs) ys, Just n) else (c, Nothing)) (zip c4 [1..])
        c4'' = map fst c4'
        ds = map (\n -> var1 $ "dummy_" ++ show n) $ catMaybes $ map snd c4'
        dConstraint = C Eql ds [constant 0]

    -- O_ik + 1 >= Xij + Xkj
    let cOverlap = [ [ var1 $ "O_" ++ show i ++ '_' : show k, constant 1 ]
                     `geq`
                     [ var1 $ "X_" ++ show i ++ '_' : j
                     , var1 $ "X_" ++ show k ++ '_' : j ]
                   | ai@(i, _) <- as
                   , ak@(k, _) <- as
                   , not $ ai == ak
                   , (j, _, _) <- intersectStations (validStations ai r env)
                                                    (validStations ak r env)
                   ]

    -- (5) E_i >= E_k + SUM (D_ij * X_ij) - M (1 - O_ik) - M Y_ik
    let c5 = [ [ var1 $ "E_" ++ show i ]
               `geq`
                ( sumdx ai ++
                [ var1 $ "E_" ++ show k
                , constant $ negate bigM
                , var (bigM, "O_" ++ show i ++ ('_' : show k))
                , var (negate bigM, "Y_" ++ show i ++ ('_' : show k)) ] )
                | ai@(i, _) <- as
                , ak@(k, _) <- as
                , not $ ai == ak
                , not $ intersectStations
                    (validStations ai r env)
                    (validStations ak r env) == [] 
                ]

    -- (6) E_k >= E_i + SUM (D_kj * X_kj) - M (1 - O_ik) - M (1 - Y_ik)
    let c6 = [ [ var1 $ "E_" ++ show k ]
                `geq`
                ( sumdx ak ++
                [ var1 $ "E_" ++ show i
                , constant $ negate bigM
                , var (bigM, "O_" ++ show i ++ ('_' : show k))
                , constant $ negate bigM
                , var (bigM, "Y_" ++ show i ++ ('_' : show k)) ] )
                | ai@(i, _) <- as
                , ak@(k, _) <- as
                , not $ ai == ak
                , not $ intersectStations
                    (validStations ai r env)
                    (validStations ak r env) == [] 
                ]

    -- (7) bin X_ij
    let c7 = BinC
            [ (1, "X_" ++ show i ++ '_' : j) | a@(i, _) <- as
                                             , (j, _) <- validStations a r env
                                             ]

    -- (8) bin Y_ik
    let c8 = BinC
            [ (1, "Y_" ++ show i ++ ('_' : show k)) | ai@(i, _) <- as
                                                    , ak@(k, _) <- as
                                                    , not $ ai == ak
                                                    , not $ intersectStations
                                                        (validStations ai r env)
                                                        (validStations ak r env) == [] ]

    -- (9) bin O_ik
    let c9 = BinC
            [ (1, "O_" ++ show i ++ ('_' : show k)) | ai@(i, _) <- as
                                                    , ak@(k, _) <- as
                                                    , not $ ai == ak
                                                    , not $ intersectStations
                                                        (validStations ai r env)
                                                        (validStations ak r env) == [] ]

    --let constraints = c1 ++ c2 ++ c3 ++ c4'' ++ (dConstraint : cOverlap) ++ c5 ++ c6 ++ [c7] ++ [c8] ++ [c9]
    let constraints = c1 ++ c2 ++ c3 ++ c4 ++ cOverlap ++ c5 ++ c6 ++ [c7] ++ [c8] ++ [c9]
        model = Model objf constraints

    -- currently ignoring transactions

    writeLP fPath (preprocess model)