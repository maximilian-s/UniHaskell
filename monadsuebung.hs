-- contains custom implementations of the Maybe, List, Binarytree and Either.

-- Maybe

import Data.Semigroup

data MyMaybe a = Einfach a | Garnix deriving Show 

instance (Eq a) => Eq (MyMaybe a) where
    (==) (Einfach x) (Einfach y) = (x == y)
    (==) Garnix Garnix = True 
    (==) _ _ = False 

instance (Ord a) => Ord (MyMaybe a) where 
    Einfach x `compare` Einfach y = x `compare` y
    Garnix `compare` Garnix = EQ
    Garnix `compare` _ = LT
    _ `compare` Garnix = GT  

instance Functor MyMaybe where 
    fmap f (Einfach x) = Einfach (f x)
    fmap f Garnix = Garnix 

instance Applicative MyMaybe where 
    pure = Einfach 
    (Einfach f) <*> (Einfach x) = Einfach (f x)
    Garnix <*> _ = Garnix 
    _ <*> Garnix = Garnix 

instance (Semigroup a) => Semigroup (MyMaybe a) where 
    (<>) Garnix Garnix = Garnix 
    (<>) (Einfach x) Garnix = Einfach x
    (<>) Garnix (Einfach x) = Einfach x
    (<>) (Einfach x) (Einfach y) = Einfach (x <> y)

instance (Semigroup a) => Monoid (MyMaybe a) where 
    mempty = Garnix 
    mappend = (<>)

instance Monad MyMaybe where 
    return = pure 
    Garnix >>= _ = Garnix
    (Einfach x) >>= f = f x
    (Einfach x) >>  y = y 
    y >> (Einfach x) = Einfach x

-- List

data ML a = E | L a (ML a) deriving Show 

instance (Eq a) => Eq (ML a) where 
    (==) (L a E) (L b E) = a == b
    (==) (L x xs) (L y ys) = x == y && xs == ys
    (==) E E = True 
    (==) _ _ = False 

instance (Ord a) => Ord (ML a) where 
    (L x E) `compare` (L y E) = x `compare` y
    (L x xs) `compare` (L y ys) 
        | x `compare` y == EQ  = xs `compare` ys
        | otherwise = x `compare` y
    E `compare` E = EQ
    _ `compare` E = GT
    E `compare` _ = LT

instance Functor ML where 
    fmap f E = E
    fmap f (L x xs) = L (f x) (fmap f xs)

instance Applicative ML where 
    pure x = L x E 
    (L f fs) <*> (L y ys) = (L (f y) (fs <*> ys))
    (L f fs) <*> E = E
    E <*> (L y ys) = E 

instance (Semigroup a) => Semigroup (ML a) where 
    (L x xs) <> (L y ys) = L (x <> y) (xs <> ys)
    (L x xs) <> E = L x xs
    E <> (L x xs) = L x xs 
    E <> E = E

instance (Semigroup a) => Monoid (ML a) where 
    mempty = E 
    mappend = (<>) 

-- Binary Tree
    
data MBB a = LE | K a (MBB a) (MBB a)

instance (Eq a) => Eq (MBB a) where 
    (==) (K n1 l1 r1) (K n2 l2 r2) = n1 == n2 && l1 == l2 && r1 == r2
    (==) LE LE = True 
    (==) _ _ = False 

instance (Ord a) => Ord (MBB a) where 
    (K n1 l1 r1) `compare` (K n2 l2 r2) | n1 == n2 && l1 == l2 = r1 `compare` r2
                                        | n1 == n2 = l1 `compare` l2 
                                        | otherwise = n1 `compare` n2 
    LE `compare` LE = EQ
    _ `compare` LE = GT 
    LE `compare` _= LT

instance (Show a) => Show (MBB a) where 
    show (K n l r) = "{{ " ++ show (l) ++ show (n) ++ show (r) ++ " }}"
    show LE = "LE" 

instance Functor MBB where 
    fmap f (K n l r) = K (f n) (fmap f l) (fmap f r)
    fmap f LE = LE 

-- Either

data Entweder a b = Das a | DasDa b deriving Show 

instance (Eq a, Eq b) => Eq (Entweder a b) where 
    (==) (Das a1) (Das a2) = a1 == a2
    (==) (DasDa b1) (DasDa b2) = b1 == b2 
    (==) _ _ = False 

instance (Ord a, Ord b) => Ord (Entweder a b) where 
    compare (Das a1) (Das a2) = compare a1 a2 
    compare (DasDa b1) (DasDa b2) = compare b1 b2
    compare (DasDa _) _ = GT 
    compare _ (DasDa _) = LT

instance (Semigroup a, Semigroup b) => Semigroup (Entweder a b) where 
    (<>) (Das a) _ = Das a
    (<>) _ (Das a) = Das a 
    (<>) (DasDa b1) (DasDa b2) = DasDa (b1 <> b2)

instance (Monoid b) => Monoid (Entweder a b) where 
    mempty = Das mempty
    mappend = (<>)
