module Chapter_08 where

-- Exercise 8.1 (*) 
-- In a similar manner to the function add for the data type Nat below, define a recursive multiplication function mult :: Nat -> Nat -> Nat for the recursive type of natural numbers. 
-- Hint: make use of add in your definition.

data Nat = Zero | Succ Nat
    deriving (Eq, Show)

add :: Nat -> Nat -> Nat
add Zero n = n
add (Succ m) n = Succ (add m n)


mult :: Nat -> Nat -> Nat
mult Zero n = Zero
mult (Succ m) n = add n (mult m n)


-- Exercise 8.5 (**)
-- Given the type declaration for Expr below, define a higher-order function folde such that folde f g replaces each Val constructor in an expression by the function f, 
-- and each Add constructor by the function g.

data Expr = Val Int | Add Expr Expr
    deriving Show

folde :: (Int -> a) -> (a -> a -> a) -> Expr -> a
folde f g (Val n) = f n
folde f g (Add x y) = g (folde f g x) (folde f g y)

-- Note: The tests for this folde depend on the correct answer of exercise 8.6. Therefore please first answer both these questions before trying the tests. 


-- Exercise 8.6 (**)
-- Using folde, define a function eval :: Expr -> Int that evaluates an expression to an integer value, and a function size :: Expr -> Int that calculates the number of values in an expression.

eval :: Expr -> Int
eval = folde id (+)


size :: Expr -> Int
size = folde (const 1) (+)


-- Exercise 8.7 (**)
-- Provide instance definitions to make the Expr and List types an instance of the Eq typeclass.

instance Eq Expr where
    -- if we are only concerned about the value we have to use an external function to evaluate Expr
    -- x == y = eval x == eval y

    -- if we are checking of what the Expr consists
    Val n == Val m = m == n
    Add n m == Add a b = n == a && m == b
    Val n == Add a b = False
    Add a b == Val n = False

data List a = Nil | Cons a (List a)
    deriving Show

instance Eq a => Eq (List a) where
    (==) Nil Nil = True
    (==) Nil (Cons _ _) = False
    (==) (Cons _ _) Nil = False
    (==) (Cons x y) (Cons a b) = x == a && y == b