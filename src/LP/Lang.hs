{-# LANGUAGE RecordWildCards #-}

module LP.Lang where

import Data.List (intersperse)

data Model = Model
    { mObj :: ObjF
    , mConstrs :: [Constraint]
    } deriving Eq

type VarName = String
type Coeff = Float
type Var = (Coeff, VarName)

data Exp = ELit Float
         | EVar Var
         deriving Eq

instance Show Exp where
    show (ELit f) = show f
    show (EVar (f, n)) = show f ++ n

data CType = Eql | Geq | Leq
    deriving Eq

instance Show CType where
    show Eql = " = "
    show Geq = " >= "
    show Leq = " <= "

data Constraint = C CType [Exp] [Exp]
                | BinC [Var]
                | IntC [Var]
                deriving Eq

instance Show Constraint where
    show (C t ls rs) = sumExps ls ++ show t ++ sumExps rs ++ ";"
    show (BinC vs) = "bin " ++ intBinHelper vs
    show (IntC vs) = "int " ++ intBinHelper vs

intBinHelper :: [Var] -> String
intBinHelper vs = concat (intersperse "," $ map snd vs) ++ ";"

data ObjDir = ObjMin | ObjMax deriving Eq

instance Show ObjDir where
    show ObjMin = "min"
    show ObjMax = "max"

data ObjF = ObjF ObjDir [Exp]
    deriving Eq

instance Show ObjF where
    show (ObjF dir vs) = show dir ++ ": " ++ sumExps vs ++ ";"

sumExps :: [Exp] -> String
sumExps [] = ""
sumExps [x] = show x
sumExps (x:y:xs) = show x ++ op ++ sumExps (y':xs)
    where
        op = if isNeg y then " - " else " + "
        y' = if isNeg y then toPos y else y

isNeg :: Exp -> Bool
isNeg (ELit f) = f < 0
isNeg (EVar (f, n)) = f < 0

toPos :: Exp -> Exp
toPos (ELit f) = ELit $ abs f
toPos (EVar (f, n)) = EVar $ ((abs f), n)

geq :: [Exp] -> [Exp] -> Constraint
geq ls rs = C Geq ls rs

leq :: [Exp] -> [Exp] -> Constraint
leq ls rs = C Leq ls rs

eql :: [Exp] -> [Exp] -> Constraint
eql ls rs = C Eql ls rs

showModel :: Model -> String
showModel Model {..} = let ls = show mObj : map show mConstrs
                        in concat $ intersperse "\n" ls

preprocess :: Model -> Model
preprocess m = let m' = preprocess' m
                in if m == m' then m' else preprocess m'

preprocess' :: Model -> Model
preprocess' Model {..} = Model mObj mConstrs'
    where
        mConstrs' = remVars vs cs
        (cs, vs) = fixConstrs mConstrs

remVars :: [VarName] -> [Constraint] -> [Constraint]
remVars _ [] = []
remVars vs (C t ls rs : cs) = C t (remVars' ls) (remVars' rs) : remVars vs cs
    where
        remVars' xs = [ case x of
                            EVar (f, n) ->
                                if n `elem` vs
                                    then ELit f
                                    else x
                            _ -> x
                        | x <- xs ]
remVars vs (c:cs) = case c of
    BinC xs -> let xs' = filterVars xs
                in if xs' == [] then cs' else BinC xs' : cs'
    IntC xs -> let xs' = filterVars xs
                in if xs' == [] then cs' else IntC xs' : cs'
    where
        cs' = remVars vs cs
        filterVars xs = filter (\x -> not $ snd x `elem` vs) xs
                    
fixConstrs :: [Constraint] -> ([Constraint], [VarName])
fixConstrs cs = go cs []
    where
        go [] vs = ([], vs)
        go (C Eql [EVar (f, n)] [ELit 1] : cs) vs = go cs (n : vs)
        go (C Geq [EVar (1, n), ELit 1] [ELit 1, ELit 1] : cs) vs = go cs (n : vs)
        go (c:cs) vs = let (cs', vs') = go cs vs
                        in (c:cs', vs')

writeLP :: FilePath -> Model -> IO ()
writeLP fPath Model {..} = do
    let ls = show mObj : map show mConstrs
        fContents = concat $ intersperse "\n" ls
    writeFile fPath fContents

var :: Var -> Exp
var = EVar

var1 :: String -> Exp
var1 n = EVar (1, n)

varNeg1 :: String -> Exp
varNeg1 n = EVar (-1, n)

constant :: Float -> Exp
constant = ELit

constantNeg :: Float -> Exp
constantNeg = constant . negate