import Data.List

------------------ Exercise One ------------------ `mod`
--foldr, filter, map

all' :: (a -> Bool) -> [a] -> Bool
all' f list = foldl (&&) True (map f list)

all'' :: (a -> Bool) -> [a] -> Bool
all'' f list = foldl (||) True (map f list)

--takeWhile' :: (a -> Bool) -> [a] -> [a]
--takeWhile' f xs
--        | 

------------------ Exercise Two ------------------

dec2Int :: [Int] -> Int
dec2Int xs = foldl (\x y -> x * 10 + y ) 0 xs

----------------- Exercise Three -----------------

curry' :: ((a,b) -> c) -> (a -> b -> c)
curry' f = \a b -> f(a,b)

------------------ Exercise Four -----------------


------------------ Exercise Five -----------------

altMap :: (a -> b) -> (a -> b) -> [a] -> [b] -- Input: altMap (+10) (+100) [0,1,2,3,4] will return [10,101,12,103,14]
altMap f g xs = reverse (helperMap f g xs (length xs - 1))



helperMap :: (a -> b) -> (a -> b) -> [a] -> Int -> [b]
helperMap f g (x:xs) n 
            | n == 0 = f x : []
            | n `mod` 2 /= 0 = helperMap f g xs (length xs - 1) ++ g x : []  
            | n `mod` 2 == 0 = helperMap f g xs (length xs - 1) ++ f x : []

------------------ Exercise Six ------------------
            
luhnDouble :: Int -> Int
luhnDouble x 
        | x * 2 > 9 = x * 2 - 9
        | otherwise = x * 2

luhn :: [Int] -> Bool
luhn xs
        | total `mod` 10 == 0 = True
        | otherwise = False
    where
        total = sum (helperLuhn xs)

helperLuhn :: [Int] -> [Int]
helperLuhn (x:xs)
        | length xs == 0 = x : []
        | length xs `mod` 2 ==0 = helperLuhn xs ++ x : []
        | otherwise =  helperLuhn xs ++ luhnDouble x : []

------------------ Exercise Seven ------------------

data Tree a = Leaf | Node (Tree a) a (Tree a) deriving Show

helperTree :: [a] -> Tree a
helperTree [] = Leaf
helperTree xs = Node (helperTree ls) x (helperTree rs)
                    where (ls,x,rs) = halve xs

toTree :: Ord a => [a] -> Tree a
toTree xs = helperTree c_xs
    where c_xs = sort xs

halve :: [a] -> ([a],a,[a])
halve xs = (take mid xs, xs !! mid, drop (mid + 1) xs)
       where
        mid = length xs `div` 2

-- TEST GIVEN FROM SOLUTIONS

heights Leaf = [0]
heights (Node l x r) = map (1+) (heights l ++ heights r)

isBalanced :: Tree a -> Bool
isBalanced t = Main.all' (<2) [ h1-h2 | h1 <- heights t , h2 <- tail $ heights t]

------------------ Exercise Eight ------------------

data Nat = Zero | Succ Nat deriving (Eq,Ord,Show,Read)

even' :: Nat -> Bool
even' x = my_number `mod` 2 == 0
        where my_number = number x

number :: Nat -> Int
number Zero = 0
number (Succ x) = number x + 1 

odd' :: Nat -> Bool
odd' x = my_number `mod` 2 /= 0
        where my_number = number x

add' :: Nat -> Nat -> Nat
add' x y = getNat(my_number_1 + my_number_2)
        where my_number_1 = number x
              my_number_2 = number y

getNat :: Int -> Nat
getNat 0 = Zero
getNat x = Succ (getNat (x-1))

mult' :: Nat -> Nat -> Nat
mult' x y = getNat(my_number_1 + my_number_2)
        where my_number_1 = number x
              my_number_2 = number y