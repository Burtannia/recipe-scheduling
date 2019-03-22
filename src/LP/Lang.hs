module LP.Lang where

import Data.List (intersperse)

type VarName = String
type Coeff = Float

data Var = Var Coeff VarName

instance Show Var where
    show (Var f n) = show f ++ n

data CType = Eql | Geq | Leq

instance Show CType where
    show Eql = " = "
    show Geq = " >= "
    show Leq = " <= "

data Constraint = C CType [Var] [Var]
                | BinC [Var]
                | IntC [Var]

instance Show Constraint where
    show (C t ls rs) = sumVars ls ++ show t ++ sumVars rs ++ ";"
    show (BinC vs) = "bin " ++ intBinHelper vs
    show (IntC vs) = "int " ++ intBinHelper vs

intBinHelper :: [Var] -> String
intBinHelper vs = concat (intersperse "," $ map varName vs) ++ ";"

data ObjDir = ObjMin | ObjMax

instance Show ObjDir where
    show ObjMin = "min"
    show ObjMax = "max"

data ObjF = ObjF ObjDir [Var]

instance Show ObjF where
    show (ObjF dir vs) = show dir ++ ": " ++ sumVars vs ++ ";"

sumVars :: [Var] -> String
sumVars [] = ""
sumVars [x] = show x
sumVars (x:y:xs) = show x ++ op ++ sumVars (y':xs)
    where
        op = if negCoeff y then " - " else " + "
        y' = if negCoeff y then toPos y else y

negCoeff :: Var -> Bool
negCoeff (Var f _) = f < 0

toPos :: Var -> Var
toPos (Var f n) = Var (abs f) n

varName :: Var -> String
varName (Var _ n) = n