module Chapter_03 where
import Data.List (groupBy)

-- Exercise 3.1
-- What are the types of the following values?

-- e3_1_1 = ['a','b','c']
-- e3_1_1 :: [Char]

-- e3_1_2 = ('a','b','c')
-- e3_1_2 :: (Char, Char, Char)

-- e3_1_3 = [(False,'O'),(True,'1')]
-- e3_1_3 :: [(Bool, Char)]

-- e3_1_4 = ([False,True],['0','1'])
-- e3_1_4 :: ([Bool], [Char])

-- e3_1_5 = [tail, init, reverse]
-- e3_1_5 :: [[a] -> [a]]


-- Exercise 3.2
-- Write down definitions that have the following types; it does not matter what the definitions actually do as long as they are type correct. The type of your defined function may be more general than the types defined below.


-- bools :: [Bool]
-- bools = map even [1,2,3]

-- nums :: [[ Int ]]
-- nums = groupBy (<=) [2^62,2,2,3,1,2,0,4,5,2]

-- add :: Num a => a -> a -> a -> a
-- add x y z = x + y + z 

-- copy :: b -> (b, b)
-- copy b = (b, b)

-- apply :: (t1 -> t2) -> t1 -> t2
-- apply f = f  


-- Exercise 3.3 (**)
-- What are the types of the following functions?

-- Hint: take care to include the necessary class constraints in the types if the functions are defined using overloaded operators.

-- second xs = head (tail xs) 
-- second :: [a] -> a

-- swap (x,y) = (y,x)
-- swap :: (a, b) -> (b, a)

-- pair x y = (x,y)
-- pair :: a -> b -> (a,b) 

-- double x = x*2
-- double :: Num a => a -> a


-- palindrome xs = reverse xs == xs 
-- palindrome :: Eq a => [a] -> Bool

-- twice f x = f (f x)
-- twice :: (a -> a) -> a -> a


-- Exercise 3.4 (*)
-- Check your answers to the preceding three questions using GHCi.

-- Copy and paste your ghci sesssion into the block comment below.
{-
$ ghci src/test.hs
GHCi, version 8.10.7: https://www.haskell.org/ghc/  :? for help
[1 of 1] Compiling Main             ( src/test.hs, interpreted )
Ok, one module loaded.
*Main> :t second
second :: [a] -> a
*Main> :t swap
swap :: (a, b) -> (b, a)
*Main> :t pair
pair :: a -> b -> (a, b)
*Main> :t double
double :: Num a => a -> a
*Main> :t palindrome
palindrome :: Eq a => [a] -> Bool
*Main> :t twice
twice :: (a -> a) -> a -> a
-}

-- Exercise 3.5 (**)
-- Why is it not feasible in general for function types to be instances of the Eq class? When is it feasible? Hint: two functions of the same type are equal if they always return equal results for equal arguments.

-- Type ypur answer into the block comment below.
{-
Because of possible side effects of functions. A function might be equal to another according to the above definition but apply some side effects. 
So the return value might be equal but what the function actually does could be something else. 
Therefore only pure functions can be compared for equality, or in other words the whole logic of the function needs to be equal, not just the arguments and return value.
Also if a function accepts an infinite amount of inputs proving the equality of the function to another by comparing their in and outputs wouldnt be feasable and in such a case a proof 
of their logical equivalence would be required.
-}
