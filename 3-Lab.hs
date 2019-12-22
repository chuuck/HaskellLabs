------------------ Exercise One ------------------

sumOfOdd = sum [ x^2 | x <- [1..100], x `mod` 2 /= 0]
sumOfEven = sum [ x^3 | x <- [1..100], x `mod` 2 == 0]

------------------ Exercise Two ------------------

grid :: Int -> Int -> [(Int,Int)]
grid x y = [(x,y) | x <- [0..x], y <- [0..y]]

square :: Int -> [(Int,Int)]
square n = [(x,y)| x <- [0..n], y <- [0..n], x /= y]

----------------- Exercise Three -----------------

replicate' :: Int -> a -> [a]
replicate' n x = [ x | nothing <- [1..n]]

----------------- Exercise Four ------------------

pyths :: Int -> [(Int, Int, Int)]
pyths n = [(x,y,z) | x <- [1..n], y <- [1..n], z <- [1..n], (x^2)+(y^2)==(z^2)]

----------------- Exercise Five ------------------

factors :: Int -> [Int]
factors m = [ x | x <- [1..m], x /= m , m `mod` x == 0]

perfect :: Int -> [Int]
perfect n = [ x | x <- [1..n], sum (factors x) == x]

------------------ Exercise Six ------------------

positions :: Eq a => a -> [a] -> [Int]
positions n xs = find n my_list  
           where my_list = zip xs [0..lngth_list]
                 lngth_list = length xs  



find :: Eq a => a -> [ (a,b)] -> [b]
find k t = [ v | (k',v) <- t, k==k']

----------------- Exercise Seven -----------------

scalarProduct :: [Int] -> [Int] -> Int
scalarProduct xs ys = sum [ fst m * snd m | m <- (zip xs ys)]

----------------- Exercise Eight -----------------

euclid :: Int -> Int -> Int
euclid a b 
    | a < b = head (reverse [ x | x <- [1..a], b `mod` x == 0 && a `mod` x == 0])
    | a > b = head (reverse [ x | x <- [1..b], b `mod` x == 0 && a `mod` x == 0])
    | otherwise = a


----------------- Exercise Nine -----------------
-- (NOT FINISHED)

merge :: Ord a => [a] -> [a] -> [a]
merge xs ys = reverse (merge' xs ys)

merge' :: Ord a => [a] -> [a] -> [a]
merge' [] ys = reverse ys
merge' xs [] = reverse xs

merge' (x:xs) (y:ys) 
    | x > y = (merge' (x:xs) (ys)) ++ y : []
    | x < y = (merge' (xs) (y:ys)) ++ x : []
    | x == y = (merge' (xs) (y:ys)) ++ x : []


--mergeSort :: Ord a => [a] -> [a]


halve :: [a] -> ([a],[a])
halve xs = (x,y)
        where
            x = take mid xs
            y = drop mid xs
            mid = length xs `div` 2 

