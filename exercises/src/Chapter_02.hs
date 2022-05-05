module Chapter_02 where


-- Exercise 2.1 (*)
-- Work through the examples of chapter ”First Steps” using GHCi.

-- Copy and paste your ghci sesssion into the block comment below.
{-
Prelude> 2 + 3 * 4
14
Prelude> (2+3)*4
20
Prelude> sqrt(3^2 + 4^2)
5.0
Prelude>  head [1,2,3,4,5]
1
Prelude> tail [1,2,3,4,5]
[2,3,4,5]
Prelude> [1,2,3,4,5] !! 2
3
Prelude>  take 3 [1,2,3,4,5]
[1,2,3]
Prelude> drop 3 [1,2,3,4,5]
[4,5]
Prelude> length [1,2,3,4,5]
5
Prelude> sum [1,2,3,4,5]
15
Prelude> product [1,2,3,4,5]
120
Prelude> [1,2,3] ++ [4,5]
[1,2,3,4,5]
Prelude> reverse [1,2,3,4,5]
[5,4,3,2,1]
Prelude> ghci src/test.hs

<interactive>:14:1: error:
    Variable not in scope: ghci :: t0 -> a -> c

<interactive>:14:6: error: Variable not in scope: src

<interactive>:14:10: error: Variable not in scope: test :: b0 -> c

<interactive>:14:15: error: Variable not in scope: hs :: a -> b0
Prelude> src/tset.hs

<interactive>:15:1: error: Variable not in scope: src :: a -> c

<interactive>:15:5: error: Variable not in scope: tset :: b0 -> c

<interactive>:15:10: error: Variable not in scope: hs :: a -> b0
Prelude> :r
Ok, no modules loaded.
Prelude> :l src/test.hs
[1 of 1] Compiling Main             ( src/test.hs, interpreted )
Ok, one module loaded.
*Main> quadruple 10
40
*Main> take (double 2) [1,2,3,4,5]
[1,2,3,4]
*Main> :r
[1 of 1] Compiling Main             ( src/test.hs, interpreted )
Ok, one module loaded.
*Main> factorial 10
3628800
*Main> average [1,2,3,4,5]
3
-}

-- Exercise 2.2 (*)
-- Parenthesise the following numeric expressions:
-- 2^3*4 2*3+4*5 2+3*4^5

e_2_2_a :: Int
e_2_2_a = (2^3)*4

e_2_2_b :: Int
e_2_2_b = (2*3)+(4*5)

e_2_2_c :: Int
e_2_2_c = 2+(3*(4^5))

