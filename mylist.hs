-- Implementation of a custom List Type and of custom function for typical usage with Lists (i.e. head, tail, appending etc.)


data ML a = E | L a (ML a) deriving Show
myList = L 1 (L 2 (L 3 (L 4 E)))

myHead :: ML a -> a
myHead E = error "empty List"
myHead (L x _) = x

myTail :: ML a -> ML a
myTail E = error "empty List"
myTail (L x (L a b)) = L a b 

myAdd :: Num a => ML a -> ML a -> ML a
myAdd _ E = E
myAdd E _ = E
myAdd (L x E) (L y E) = L (x+y) E
myAdd (L x (L a b)) (L y (L c d)) = L (x+y) (myAdd (L a b) (L c d))

myAppend :: ML a -> ML a -> ML a
myAppend E (L d e) = L d e
myAppend (L x E) (L d e) = L x (myAppend E (L d e))
myAppend (L x (L a b)) (L d e) = L x (myAppend (L a b) (L d e))

toString :: Show a => ML a -> String
toString (L x E) = show x
toString (L x (L a b)) = show x ++ ", " ++ toString(L a b)
