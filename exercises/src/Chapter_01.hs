module Chapter_01 where

-- Exercise 1.1 (*)
-- Give another possible calculation for the result of double (double 2).


-- Complete the following block comment.
{-
double (double 2)
= {applying double}
double( 2 + 2 )
...
-}


-- Exercise 1.2 (*)
-- Show that sum [x] = x for any number x.

-- Complete the following block comment.
{-
sum [x]
= {applying sum}
x + sum[]
= {applying sum}
x + 0
= x
-}


-- Exercise 1.3 (*)
-- Define a function myProduct that produces the product of a list of numbers, and show using your definition that myProduct [2,3,4] == 24.
-- Note: We use the name "myProduct" since the name product is already defined in the ghc Prelude.

myProduct :: Num p => [p] -> p
myProduct [] = 1
myProduct (n:ns) = n * myProduct ns


{-
myProduct [2,3,4]
= {applying myProduct}
2 * myProduct [3,4]
= {applying myProduct}
2 * 3 * myProduct [4]
= {applying myProduct}
2 * 3 * 4 * myProduct []
= {applying myProduct}
2 * 3 * 4 * 1
= {applying *}
24
-}


