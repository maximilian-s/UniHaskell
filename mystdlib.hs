-- Custom implementation of commonly used functions of the ghc standard library --

any' :: (a -> Bool) -> [a] -> Bool
any' f xs = foldl (\acc x -> if f x then True else acc) False xs


all' :: (a -> Bool) -> [a] -> Bool
all' f xs = foldl (\acc x -> if (f x) == acc then acc else True) False xs

map' :: (a -> b) -> [a] -> [b]
map' f xs = foldr (\x acc -> (f x) :acc) [] xs
