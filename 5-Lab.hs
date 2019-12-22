

------------- Exercise One --------------------

-- Task: find if the element is in the BST
data Tree a = Leaf a | Node (Tree a) a (Tree a)

occurs :: Ord a => a -> Tree a -> Bool
occurs x (Leaf a) = x == a

occurs x (Node ls a rs)
    | compare x a == LT = occurs x ls
    | compare x a == GT = occurs x rs
    | otherwise = True

------------- Exercise Two --------------------



------------ Exercise Three -------------------