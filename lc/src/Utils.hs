module Utils where
import Data.Char ( isNumber )

removeItem :: Eq a => a -> [a] -> [a]
removeItem _ [] = []
removeItem a (x:xs)
    | a == x  = removeItem a xs
    | otherwise = x : removeItem a xs

removeDuplicates :: Eq a => a -> [a] -> [a]
removeDuplicates a [] = [a]
removeDuplicates a (x:xs)
    | a == x  = removeDuplicates a xs
    | otherwise = x: removeDuplicates a xs

toSet :: Eq a => [a] -> [a]
toSet [] = []
toSet a = foldr removeDuplicates a a

splitByTailNumbers :: String -> (String, String) 
splitByTailNumbers [] = ([], [])
splitByTailNumbers a = (reverse (dropWhile isNumber (reverse a)) , reverse (takeWhile isNumber (reverse a)))

