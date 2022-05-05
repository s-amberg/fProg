module LCInterpreter
    (
        Term(..),
        freeVars,
        substitute,
        incrementVar,
        reduce,
        derivation,
        leftmostInnermost,
        leftmostOutermost
    ) where

import Utils ( removeItem, toSet, splitByTailNumbers )
import Text.Printf ( printf )

type Id = String

data Term = Var Id   -- Variables
    | Abs Id Term    -- Abstractions
    | App Term Term  -- Applications


instance Show Term where
    show (Var a) = a
    show (Abs a t) = printf "(\\%s. %s)" a (show t)
    show (App s (App a b)) = printf "%s (%s %s)" (show s) (show a) (show b)
    show (App s t) = printf "%s %s" (show s) (show t)


-- Implement a function freeVars t that returns a set of all free variables within a lambda term t. You may want to represent sets as lists without duplicates.
-- Appendix A.2 of the lecture notes contains recursive definitions of non-freeness that you may find useful.
-- does not rename, so if y is multiple free Vars, only one y is returned, aswell as if y is bound but also free more outward
freeVars:: Term ->  [Id]
freeVars (Var a) = [a]
freeVars (Abs a t) = removeItem a (freeVars t)
freeVars (App a b) = toSet (freeVars a ++ freeVars b)

allVars:: [Term] -> [Id]
allVars as = toSet (foldr ((++) . allVarsOfTerm) [] as)

allVarsOfTerm:: Term -> [Id]
allVarsOfTerm (Var a) = [a]
allVarsOfTerm (Abs a t) = toSet (a : allVarsOfTerm t)
allVarsOfTerm (App a b) = toSet (allVarsOfTerm a ++ allVarsOfTerm b)


nfin:: Id -> Term -> Bool
nfin x (Var a) = x /= a
nfin x (Abs a t) = x /= a && nfin x t
nfin x (App a b) = nfin x a && nfin x b


-- Implement a function substitute (x,tx) t that replaces all free occurrences of the variable x within the term t with the term tx. 
-- Take care to avoid capturing substitutions (you will have to do some alpha renaming with fresh variables to avoid this). 
-- Appendix A.2 of the lecture notes contains a recursive definition of substitution that you may find useful. 
-- In case you find the task of avoiding variable capture too challenging, skip this and only use terms with unique bound and free variable names.
substitute :: Id -> Term -> Term -> Term
substitute x tx (Var y)
    | nfin x (Var y) = Var y
    | otherwise  = tx
substitute x tx (Abs y t)
    | not(nfin x (Var y)) = Abs y t
    | nfin x (Var y) && nfin y tx = Abs y (substitute x tx t)
    | otherwise = substitute x tx (safeAConversion y [] (Abs y t) [(Abs y t),tx])
substitute x tx (App ta tb) = App (substitute x tx ta) (substitute x tx tb)

-- a Conversion for Abstraction of \x.M where x may not occur in M or L, returns Abstraction
-- substitute innermost term first with new Var of outer => inner has y2, outer y1 etc.
-- search for variable not in array of terms and do a-conversion, first term is target to replace Id in
safeAConversion :: Id -> [Id] -> Term -> [Term] -> Term
safeAConversion a ids ta [] = ta
safeAConversion a ids (Abs a2 ta) tbs
    | a == a2 = Abs newA (safeAConversion a (ids ++ [newA]) substituted tbs)
    | otherwise = Abs a2 (safeAConversion a ids ta tbs) where
        newA = newVar a (allVars tbs ++ ids)
        substituted = substitute a (Var newA) ta
safeAConversion a ids (Var b) tbs = aConversion a a (Var b)
safeAConversion a ids (App ta ta2) tbs = App (safeAConversion a ids ta tbs) (safeAConversion a ids ta2 tbs)

newVar :: Id -> [Id] -> Id
newVar a [] = a
newVar a (b:bs)
    | a == b = newVar (incrementVar a) (b:bs)
    | otherwise = newVar a bs

incrementVar :: Id -> Id
incrementVar a = name ++ incrementString number where
    (name, number) = splitByTailNumbers a
    incrementString "" = "1"
    incrementString s = show ((read s :: Int) + 1)

-- target id (2nd) mustn't exist in term
aConversion :: Id -> Id -> Term -> Term
aConversion s t (Var a)
    | a == s = Var t
    | otherwise = Var a
aConversion s t (Abs a ta)
    | a == s = Abs s (aConversion s t ta)
    | otherwise  = Abs a (aConversion s t ta)
aConversion s t (App ta tb) = App (aConversion s t ta) (aConversion s t tb)

-- Implement a function isBetaRedex t which returns True iff the top level of the term t is a beta redex.
isBetaRedex :: Term -> Bool
isBetaRedex (Var x) = False
isBetaRedex (Abs x a) = False
isBetaRedex (App (Abs x a) _) = True
isBetaRedex (App _ _) = False

redexDepth :: (Num a, Ord a) => Term -> a -> a
redexDepth (Var x) i = i
redexDepth (Abs x a) i = redexDepth a i
redexDepth (App a b) i = if isBetaRedex (App a b) then max (redexDepth a (i+1)) (redexDepth b (i+1)) else max (redexDepth a i) (redexDepth b i)

-- Use substitute to implement a function betaReduce t that applies a beta reduction to top level of the term t.
betaReduce :: Term -> Term
betaReduce (App (Abs x a) b) = substitute x b a
betaReduce a = a

-- performs a single reduction step using the leftmost innermost evaluation strategy.
leftmostInnermost :: Term -> Term
leftmostInnermost (Var x) = Var x
leftmostInnermost (Abs x a) = Abs x (leftmostInnermost a)
leftmostInnermost (App (Abs x a) b)
    | lDepth >= rDepth && lDepth == 1 = betaReduce (App (Abs x a) b)
    | lDepth >= rDepth = App (leftmostInnermost (Abs x a)) b
    | otherwise = App (Abs x a) (leftmostInnermost b)
        where
            lDepth = redexDepth a 1
            rDepth = redexDepth b 1
leftmostInnermost (App a b)
    | lDepth >= rDepth = App (leftmostInnermost a) b
    | otherwise = App a (leftmostInnermost b)
        where
            lDepth = redexDepth a 0
            rDepth = redexDepth b 0

-- performs a single reduction step using the leftmost outermost evaluation strategy.
leftmostOutermost :: Term -> Term
leftmostOutermost (Var x) = Var x
leftmostOutermost (Abs x a) = Abs x (leftmostOutermost a)
leftmostOutermost (App (Abs x a) b) = betaReduce (App (Abs x a) b)
leftmostOutermost (App a b)
    | lDepth > 0 = App (leftmostOutermost a) b
    | rDepth > 0 = App a (leftmostOutermost b)
    | otherwise = App a b
        where
            lDepth = redexDepth a 0
            rDepth = redexDepth b 0

-- performs a step by step derivation using a given reduction strategy.
derivation :: (Term -> Term) -> Term -> [Term]
derivation strategy term 
    | redexDepth term 0 /= 0 = (derivation strategy (strategy term)) ++ [term]
    | otherwise = [term]



-- returns the beta normal form of a given term in case it exists. (Minimal Goal)
reduce :: Term -> Term
reduce a
    | redexDepth a 0 /= 0 = reduce (leftmostOutermost a)
    | otherwise = a

