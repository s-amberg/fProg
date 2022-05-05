{-# OPTIONS_GHC -Wno-incomplete-patterns #-}

module Chapter_06 where


-- Exercise 6.2 (*)
-- Define a recursive function sumdown :: Int -> Int that returns the sum of the non-negative integers from a given non-negative integer to zero. For example, sumdown 3 should return the result 3 + 2 + 1 + 0 = 6.

sumdown :: Int -> Int
sumdown 0 = 0
sumdown n = n + sumdown (n - 1)


-- Exercise 6.3 (*)
-- Define the exponentiation operator ^ for non-negative integers using the same pattern of recursion as the multiplication operator *, and show how the expression 2 ^ 3 is evaluated using your definition.
-- Note: If you are trying this within ghc, please note that ^ is already defined in the Prelude. Use the operator ^# which is currently undefined instead.

(^#) ::  Int -> Int -> Int
_ ^# 0 = 1
0 ^# _ = 0
x ^# y = x * (x ^# (y-1))
-- (^#) = \x -> \y -> x * (x ^# (y-1))
-- (^#) x y = x * (x ^# (y-1))


{-
2^3
= {applying ^}
2 * (2^2)
= {applying ^}
2 * 2 * (2^1)
= {applying ^}
2 * 2 * 2 * (2^0)
= {applying ^}
2 * 2 * 2 * 1
= {applying *}
8
-}



-- Exercise 6.5 (**)
-- Using the recursive definitions given in chapter ”Recursive Functions”,
-- show how length [1,2,3], drop 3 [1,2,3,4,5], and init [1,2,3] are evaluated.

-- length :: [a] -> Int
-- length [] = 0
-- length (_ : xs) = 1 + length xs

-- drop :: Int -> [a] -> [a]
-- drop 0 xs = xs
-- drop _ [] = []
-- drop n (_ : xs) = drop (n - 1) xs

-- init :: [a] -> [a]
-- init [_] = []
-- init (x:xs) = x : init xs

-- No executable code required.
-- Enter your solution within the block comment below.

{-
length [1,2,3]
= {applying length}
1 + length [2,3]
= {applying length}
1 + 1 + length [3]
= {applying length}
1 + 1 + 1 + length []
= {applying length}
1 + 1 + 1 + 0
= {applying +}
3


drop 3 [1,2,3,4,5]
= {applying drop}
drop 2 [2,3,4,5]
= {applying drop}
drop 1 [3,4,5]
= {applying drop}
drop 0 [4,5]
= {applying drop}
[4,5]


init [1,2,3]
= {applying init}
1 : init[2,3]
= {applying init}
1 : 2 : init[3]
= {applying init}
1 : 2 : []
= {list representation}
[1,2]

-}


-- Exercise 6.6 (**)
-- Without looking at the definitions from the standard prelude, define the following library functions on lists using recursion.
-- Note: If you are trying this within ghc, please note that these functions are already defined in the Prelude. Use fresh function names by prefixing each name with a "my" and changing the resulting name into camelcase, for example "and" becomes "myAnd". The operators you define should be suffixed with a "#" (i.e., !! becomes !!#).
-- Note: Most of these functions are defined in the prelude using other library functions rather than using explicit recursion, and are generic functions rather than being specific to the type of lists.

-- (a) Decide if all logical values in a list are True: 
--     and :: [Bool] -> Bool


myAnd :: [Bool] -> Bool
myAnd [] = True 
myAnd (b:bs) = b && myAnd bs


-- (b) Concatenate a list of lists:
--     concat :: [[a]] -> [a]

myConcat :: [[a]] -> [a]
myConcat [] = []
myConcat (xs:xss) = xs ++ myConcat xss


-- (c) Produce a list with a non-negative number of identical elements: 
--     replicate :: Int -> a -> [a]

myReplicate :: Int -> a -> [a]
myReplicate 0 _ = []
myReplicate n e = [e] ++ myReplicate (n-1) e


-- (d) Select the nth element of a list: 
--     (!!) :: [a] -> Int -> a

(!!#) :: [a] -> Int -> a
xs !!# 0 = head xs
[] !!# _ = undefined
(!!#) (x:xs) n = xs !!# (n-1)



-- (e) Decide if a value is an element of a list:
--     elem :: Eq a => a -> [a] -> Bool

myElem :: Eq a => a -> [a] -> Bool
myElem a [] = False
myElem a (x:xs) = (a == x) || myElem a xs


-- Exercise 6.7 (**)
-- Define a recursive function merge :: Ord a => [a] -> [a] -> [a] that merges two sorted lists to give a single sorted list. For example:
-- > merge [2 ,5 ,6] [1 ,3 ,4] 
-- [1,2,3,4,5,6]
-- Note: your definition should not use other functions on sorted lists such as insert or isort, but should be defined using explicit recursion.

merge :: Ord a => [a] -> [a] -> [a]
merge [] xs  = xs
merge xs [] = xs
merge (y:ys) (x:xs) | y <= x = merge ys (y:x:xs)
                    | y > x = merge ys (x : merge [y] xs) 


-- Exercise 6.8 (**)
-- Using merge, define a function msort :: Ord a => [a] -> [a] that implements merge sort, in which the empty list and singleton lists are already sorted, and any other list is sorted by merging together the two lists that result from sorting the two halves of the list separately.
-- Hint: first define a function halve :: [a] -> ([a],[a]) that splits a list into two halves whose lengths differ by at most one.


halve :: [a] -> ([a], [a])
halve xs = (take h xs, drop h xs) where h = length xs `div` 2

msort :: Ord a => [a] -> [a]
msort [] = []
msort [a] = [a]
msort xs = merge (msort a) (msort b) where (a, b) = halve xs


