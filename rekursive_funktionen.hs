-- Exercises to practice Recursion and Tail Recusion in Haskell -- 

-- custom length function, implemented recursivly (1) and by using tail recursion (2)
length' :: [a] -> Int
length' [] = 0
length' (x:xs) = 1 + length' xs

length'' :: [a] -> Int
length'' [] = 0
length'' (x:xs) = len_help (x:xs) 0
    where len_help [] akk = akk
          len_help (x:xs) akk = len_help xs (akk+1)

-- custom function to check if a is element of List [a] 
istElem :: Eq a => [a] -> a -> Bool
istElem [] _ = False
istElem (x:xs) n
    | x == n = True
    | otherwise = istElem xs n 

-- custom function for reversing a List [a], first is recursive, second implemented by using tail recursion
reverse' :: [a] -> [a]
reverse' [] = []
reverse' (x:xs) = reverse' xs ++ x:[]

reverse'' :: [a] -> [a]
reverse'' [] = []
reverse'' (x:xs) = rev_help (x:xs) []
    where rev_help :: [a] -> [a] -> [a]
          rev_help [] akk = akk
          rev_help (x:xs) akk = rev_help xs (x:akk)

-- custom concatination function
concat' :: [a] -> [a] -> [a]
concat' [] ys = ys
concat' (x:xs) ys = x: concat' xs ys
