{-# LANGUAGE RecordWildCards #-}

module LP.Lang where

import Data.List (intersperse)

data Model = Model
    { mObj :: ObjF
    , mConstrs :: [Constraint]
    }

type VarName = String
type Coeff = Float
type Var = (Coeff, VarName)

data Exp = ELit Float
         | EVar Var

instance Show Exp where
    show (ELit f) = show f
    show (EVar (f, n)) = show f ++ n

data CType = Eql | Geq | Leq

instance Show CType where
    show Eql = " = "
    show Geq = " >= "
    show Leq = " <= "

data Constraint = C CType [Exp] [Exp]
                | BinC [Var]
                | IntC [Var]

instance Show Constraint where
    show (C t ls rs) = sumExps ls ++ show t ++ sumExps rs ++ ";"
    show (BinC vs) = "bin " ++ intBinHelper vs
    show (IntC vs) = "int " ++ intBinHelper vs

intBinHelper :: [Var] -> String
intBinHelper vs = concat (intersperse "," $ map snd vs) ++ ";"

data ObjDir = ObjMin | ObjMax

instance Show ObjDir where
    show ObjMin = "min"
    show ObjMax = "max"

data ObjF = ObjF ObjDir [Exp]

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

bigM :: Float
bigM = 9999999

-- test1 :: [Constraint]
-- test1 =
--     [ [var (1, "Xs1s2")] `leq` [constant 10]
--     , [var (1, "Xad2")] `leq` [constant 80]
--     ]

-- test2 :: [Constraint]
-- test2 =
--     [ [var (1, "Xs1d2"), var (1, "Xs1a"), var (1, "Xs1s2")] `eql` [constant 50]
--     , [var1 "Xs2a", var (-1, "Xs1s2")] `eql` [constant 40]
--     , [var1 "Xad2", var (-1, "Xs1a"), var (-1, "Xs2a")] `eql` [constant 0]
--     , [var1 "Xd1d2", varNeg1 "Xs1d1", varNeg1 "Xd2d1"] `eql` [constantNeg 30]
--     , [var1 "Xd2d1", varNeg1 "Xad2", varNeg1 "Xd1d2"] `eql` [constantNeg 60]
--     ]

-- testObj :: ObjF
-- testObj = ObjF ObjMin
--     [ var (9, "Xs1d1")
--     , var (4, "Xs1a")
--     , var (2, "Xs1s2")
--     , var (3, "Xs2a")
--     , var (1, "Xad2")
--     , var (3, "Xd1d2")
--     , var (2, "Xd2d1")
--     ]

-- testModel :: Model
-- testModel = Model testObj (test1 ++ test2)