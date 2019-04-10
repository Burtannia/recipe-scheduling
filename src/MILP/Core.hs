{-# LANGUAGE RecordWildCards #-}

{-|
This module contains the core ADT
for defining MILP problems.
-}

module MILP.Core (
    -- * Types
    Model (..), VarName, Coeff,
    Var, Term (..), CType (..),
    Constraint (..), ObjDir (..),
    ObjF (..),

    -- * Creating Models
    var, var1, varNeg1, constant,
    constantNeg, geq, leq, eql,

    -- * Writing Model
    showModel, writeLP, preprocess ) where

import Data.List (intersperse)

-- |A model consists of an objective
-- function and a list of constraints.
data Model = Model
    { mObj :: ObjF
    , mConstrs :: [Constraint]
    } deriving Eq

type VarName = String
type Coeff = Float

-- |Variables are represented as the
-- pair of a coefficient with a variable name.
type Var = (Coeff, VarName)

-- |A term is either a literal value or a variable.
-- Lists of terms (['Term']) are considered to be
-- summations of all the terms in the list.
data Term = TLit Float
          | TVar Var
          deriving Eq

instance Show Term where
    show (TLit f) = show f
    show (TVar (f, n)) = show f ++ n

-- |A constraint type can either be equality
-- or an inequality.
data CType = Eql | Geq | Leq
    deriving Eq

instance Show CType where
    show Eql = " = "
    show Geq = " >= "
    show Leq = " <= "

-- |Constraints can represent an equality
-- or inequality between two lists of terms or
-- the constraint of a list of variables to
-- binary values or to integer values.
data Constraint = C CType [Term] [Term]
                | BinC [Var]
                | IntC [Var]
                deriving Eq

instance Show Constraint where
    show (C t ls rs) = sumTerms ls ++ show t ++ sumTerms rs ++ ";"
    show (BinC vs) = "bin " ++ intBinHelper vs
    show (IntC vs) = "int " ++ intBinHelper vs

intBinHelper :: [Var] -> String
intBinHelper vs = concat (intersperse "," $ map snd vs) ++ ";"

-- |Direction of the objective function
-- is either minimize or maximize.
data ObjDir = ObjMin | ObjMax deriving Eq

instance Show ObjDir where
    show ObjMin = "min"
    show ObjMax = "max"

-- |An objective function consists of
-- a direction and a list of terms.
data ObjF = ObjF ObjDir [Term]
    deriving Eq

instance Show ObjF where
    show (ObjF dir vs) = show dir ++ ": " ++ sumTerms vs ++ ";"

sumTerms :: [Term] -> String
sumTerms [] = ""
sumTerms [x] = show x
sumTerms (x:y:xs) = show x ++ op ++ sumTerms (y':xs)
    where
        op = if isNeg y then " - " else " + "
        y' = if isNeg y then toPos y else y

isNeg :: Term -> Bool
isNeg (TLit f) = f < 0
isNeg (TVar (f, n)) = f < 0

toPos :: Term -> Term
toPos (TLit f) = TLit $ abs f
toPos (TVar (f, n)) = TVar $ ((abs f), n)

-- Writing Model

-- |Given a model creates a String in the
-- format of an LPSolve file.
showModel :: Model -> String
showModel Model {..} = let ls = show mObj : map show mConstrs
                        in concat $ intersperse "\n" ls

-- |Given a model, writes the result of 'showModel'
-- to the given path.
writeLP :: FilePath -> Model -> IO ()
writeLP fPath m@Model {..} = do
    writeFile fPath (showModel m)

-- Helper Functions

-- |Create a 'Term' from a 'Var'.
var :: Var -> Term
var = TVar

-- |Create a 'Term' from a variable
-- name with a coefficient of 1.
var1 :: String -> Term
var1 n = TVar (1, n)

-- |Create a 'Term' from a variable
-- name with a coefficient of -1.
varNeg1 :: String -> Term
varNeg1 n = TVar (-1, n)

-- |Create a 'Term' from a literal.
constant :: Float -> Term
constant = TLit

-- |Concatenation of 'constant' and 'negate'.
constantNeg :: Float -> Term
constantNeg = constant . negate

-- |Create a constraint where the sum of
-- the first list of terms is greater than
-- or equal to the second list of terms.
geq :: [Term] -> [Term] -> Constraint
geq ls rs = C Geq ls rs

-- |Create a constraint where the sum of
-- the first list of terms is less than
-- or equal to the second list of terms.
leq :: [Term] -> [Term] -> Constraint
leq ls rs = C Leq ls rs

-- |Create a constraint where the sum of
-- the first list of terms is
-- equal to the second list of terms.
eql :: [Term] -> [Term] -> Constraint
eql ls rs = C Eql ls rs

-- Preprocessor

-- |Applies a few optimisations to the given
-- model by removing redundant constraints and
-- variables.
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
                            TVar (f, n) ->
                                if n `elem` vs
                                    then TLit f
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
        go (C Eql [TVar (f, n)] [TLit 1] : cs) vs = go cs (n : vs)
        go (C Geq [TVar (1, n), TLit 1] [TLit 1, TLit 1] : cs) vs = go cs (n : vs)
        go (c:cs) vs = let (cs', vs') = go cs vs
                        in (c:cs', vs')