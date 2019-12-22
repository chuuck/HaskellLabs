------------- Exercise One ----------------------

-- Task: return last element without using last cmd (2 examples)

last' :: [a] -> a
last' xs = xs !! (length xs - 1)

last'' :: [a] -> a
last'' xs = head (reverse xs)

------------- Exercise Two ----------------------

-- Task: return the third element with different techniques
third :: [a] -> a
third xs = head (tail (tail xs))

third' :: [a] -> a
third' xs = xs !! 2

third'' :: [a] -> a
third'' (x:y:z:xs) = z


------------ Exercise Three ---------------------

-- Task: Smart tail, returns empty list if there is no tail (3 different techniquies)

safetail :: [a] -> [a]
safetail xs = if null' xs == True then [] else tail xs 

safetail' :: [a] -> [a]
safetail' xs 
        | null' xs == True = []
        | otherwise = tail xs

safetail'' :: [a] -> [a]
safetail'' [] = []
safetail'' (x:xs) = xs      

null' :: [a] -> Bool
null' xs 
        | length xs == 0 = True
        | otherwise = False

------------- Exercise Four ---------------------

-- Task: split even length list in two parts

halve :: [a] -> ([a],[a])
halve xs = (x,y)
        where
            x = take mid xs
            y = drop mid xs
            mid = length xs `div` 2 

------------- Exercise Five ---------------------

--enc :: Int -> String -> String


------------- Exercise Six ----------------------

-- Task: create an algorithm that check if 4-digit number is lugh number

luhnDouble :: Int -> Int
luhnDouble x 
        | x * 2 > 9 = x * 2 - 9
        | otherwise = x * 2


luhn :: Int -> Int -> Int -> Int ->Bool
luhn a b c d 
        | total `mod` 10 == 0 = True
        | otherwise = False
    where
        total = luhnDouble a + b + luhnDouble c + d