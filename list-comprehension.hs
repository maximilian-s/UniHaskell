-- ungeraden Elemente einer Liste -- 
oddList :: [Integer] -> [Integer]
oddList [] = []    
oddList list = [x | x <- list, mod x 2 == 1]


-- gerade Elemente und zweifaches der ungeraden Elemente einer Liste --
secondList :: [Integer] -> [Integer]
secondList [] = []
secondList list = [if mod x 2 == 0 then x else 2*x | x <- list]

 
-- natürlichen Zahlen mit Rest 5 bei der Division durch 7 -- 
thirdList :: [Integer] -> [Integer]
thirdList [] = []
thirdList list = [x | x <- list, mod x 7 == 5]


-- Länge einer Liste -- 
listLength :: [a] -> Integer
listLength [] = 0
listLength (x:xs) = 1 + listLength xs


-- dreifach -- 
dreifach :: Integer -> Integer
dreifach a = 3*a


-- nur Großbuchstaben ausgeben eines Strings -- 
nurGross :: [Char] -> [Char]
nurGross [] = []
nurGross string = [x | x <- string, elem x ['A'..'Z']]

putStrLnNurGross :: [Char] -> IO()
putStrLnNurGross = putStrLn.nurGross


-- Faktorzerlegung eines Integers -- 
faktoren :: Integer -> [Integer]
faktoren num = [x | x <- [1..num-1], mod x num == 0]


-- groesster gemeinsamer Teiler zweier Integers --
ggT :: Integer -> Integer
ggT = maximum.faktoren


-- Pythagoras Trippel --
pyth_trip :: Integer -> [(Integer,Integer,Integer)]
pyth_trip num = [(a,b,c) | a <- [0..num], b <- [0..num], c <- [0..num], (a*a) + (b*b) == c*c]
