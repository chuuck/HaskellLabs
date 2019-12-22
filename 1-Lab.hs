------------- Exercise Two ----------------------

-- Summs all elements in list
sum' [] = 0
sum' (x:xs) = x + sum' xs

------------ Exercise Three ---------------------

-- Creates product of all elements in the list
product' [] = 1
product' (x:xs) = x * product' xs

------------- Exercise Four ---------------------

-- Sort list
quicksort [] = []
quicksort (x:xs) = quicksort ls ++ [x] ++ quicksort rs
            where
                ls = [ a | a <- xs, a <= x]
                rs = [ a | a <- xs, a > x]

-- Sorts out the list, however, reverse
quicktros [] = []
quicktros (x:xs) = quicktros ls ++ [x] ++ quicktros rs
            where
                ls = [ a | a <- xs, a > x]
                rs = [ a | a <- xs, a <= x]

------------- Exercise Five ---------------------   

-- Sorts list, however, deletes duplicates
quicksort' [] = []
quicksort' (x:xs) = quicksort' ls ++ [x] ++ quicksort' rs
            where
                ls = [ a | a <- xs, a < x]
                rs = [ a | a <- xs, a > x]

-------------- Exercise Six --------------------- 

-- I don't understand this one

-------------- Exercise Seven -------------------

-- Debugging code:
--  mistake 1: `` instead of ''
--  mistake 2: spaces instead of tabs
--  mistake 3: function name has to start with lowercase
n = a `div` length xs
    where 
        a = 10 
        xs = [1,2,3,4,5]

-------------- Exercise Eight -------------------

{-|
    Types
        ['a','b','c'] -> [Char]
        ('a','b','c') -> (Char, Char, Char)
        ['a',3,True] -> Not possible
        ('a',3,True) -> (Char, Int, Bool)
        [ (False, '0'), (True,'1')] -> [(Bool, Char)]
        ( [True,False] , ['0','1'] ) -> Not Possible

        [tail, init, reverse] -> WHAT IS THIS???

        [] -> [a]
        2 : 3 : [] : 4 : 5 : [] -> [[Int]]
        [] : [] -> [[a]]
-}

-------------- Exercise Nine -------------------

bools :: [Bool]
bools = [True, False]

nums :: [[Int]]
nums = [[1], [2], [3]]

add :: Int -> Int -> Int -> Int
add x y z = x + y + z

copy :: a -> (a,a)
copy x = (x,x)

--Don't fully understand this one
apply :: (a -> b) -> a -> b
apply f x = f x

explode :: String -> [Char]
explode s = [ x | x <- s]
-- Could have written explode x = x, because String == [Char]

--------------- Exercise Ten -------------------

second :: [a] -> a
second xs = head (tail xs)

swap :: (a,b) -> (b,a)
swap (x,y) = (y,x)

pair :: a -> b -> (a,b)
pair x y = (x,y)

double :: Num a => a -> a
double x = x*2

palindrome :: Eq a => [a] -> Bool
palindrome xs = reverse xs == xs

--Don't fully understand this one
twice :: (a -> a) -> a -> a
twice f x = f ( f x )

--------------- Exercise Eleven -----------------

{-|
The class Eq is instantiated by all of the basic types as well as Lists and Tuples built from these. 
However, function types are not an instance of the Eq class. Suggest reasons why this is the case.
}

-- Don't know, need to understand this