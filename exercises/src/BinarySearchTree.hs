module BinarySearchTree (
    T, empty, insert, fromList, toList, member, merge, bubbleSort, restore
) where
    import Text.Printf ( printf )

    data T a = Leaf | Node (T a) a (T a)

    instance (Show a) => Show (T a) where
        show = showAtLevel 0
            where
                showAtLevel lv Leaf = printf "*"
                showAtLevel lv (Node l v r) = printf "%s\n%s%s\n%s" (showAtLevel (lv+1) l) (addSpace lv)  (show v ) (showAtLevel (lv+1) r)
                addSpace = flip replicate '\t'


    instance (Ord a) => Semigroup(T a ) where
        (<>) = merge

    instance (Monoid a, Ord a) => Monoid(T a ) where
        mempty = empty

    instance (Ord a, Eq a) => Ord(T a ) where
        (<=) Leaf Leaf = True
        (<=) Leaf _ = False
        (<=) _ Leaf = False
        -- compare values of end leaves
        (<=) (Node Leaf v1 Leaf) (Node Leaf v2 Leaf) =  v1 <= v2
        -- left is final right not
        (<=) (Node _ v1 Leaf) (Node l2 v2 _) = Node Leaf v1 Leaf <= l2
        -- right is final left not
        (<=) (Node _ v1 r1) (Node Leaf v2 _)= r1 <= Node Leaf v2 Leaf
        -- left and right are non final
        (<=) (Node _ v1 r1) (Node l2 v2 _)= r1 <= l2


    instance (Eq a) => Eq( T a) where
        (==) Leaf Leaf = True
        (==) Leaf _ = False
        (==) _ Leaf = False
        (==) (Node l1 v1 r1) (Node l2 v2 r2) = l1 == l2 && r1 == r2 && v1 == v2

    toList :: Ord a => T a -> [a]
    toList Leaf = []
    toList (Node l v r) = toList l ++ ( v : toList r)

    member :: Ord a => a -> T a -> Bool
    member a Leaf = False
    member a (Node l v r) = member a l || a == v || member a r

    empty :: T a
    empty = Leaf

    insert :: Ord a => a -> T a -> T a
    insert a Leaf = Node Leaf a Leaf
    insert a (Node l v r)   | a <= v = Node (insert a l) v r
                            | otherwise = Node l v (insert a r)


    fromList :: Ord a => [a] -> T a
    fromList = fromOrderedList . bubbleSort

    fromOrderedList :: Ord a => [a] -> T a
    fromOrderedList as
        | null as = Leaf
        | otherwise = Node (fromOrderedList l) v (fromOrderedList r)
            where (l,v,r) = splitList as

    splitList :: Ord a => [a] -> ([a], a, [a])
    splitList as = (l, head v, r) where (l,v,r) = recSplit ([], as, [])

    recSplit :: ([a], [a], [a]) -> ([a], [a], [a])
    recSplit (l, vs, r)
        | length vs > 2 = recSplit (l ++ [head vs], init (tail vs), last vs : r)
        | length vs == 2 = recSplit (l ++ [head vs], tail vs, r)
        | otherwise = (l,vs,r)

    merge :: Ord a => T a -> T a -> T a
    merge Leaf b = b
    merge a Leaf = a
    merge a b = fromList (bubbleSort (toList a ++ toList b))

    restore :: Ord a => T a -> T a
    restore = fromList . toList

    bubbleSort :: Ord a => [a] -> [a]
    bubbleSort = foldr bubbleUp []

    bubbleUp:: Ord a => a -> [a] -> [a]
    bubbleUp el [] = [el]
    bubbleUp el (x:xs)
        | el > x = x: bubbleUp el xs
        | otherwise = el: x: xs

