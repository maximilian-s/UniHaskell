
import Data.Semigroup

-- Complex Numbers

data ComplexNumber = CN Double Double 

instance Show ComplexNumber where
    show (CN a b) = show (a) ++ "+" ++ show(b) ++ "i" 

instance Monoid ComplexNumber where
    mempty = CN 0 0
    mappend (CN a b) (CN c d) = CN (a+c) (b+d)

-- RGB Colors

newtype RGB = RGB (Int, Int, Int) deriving Show 

instance Semigroup RGB where 
    RGB (r1, g1, b1) <> RGB (r2, g2, b2) = RGB (r1+r2, g1+g2, b1+b2)

instance Monoid RGB where 
    mempty = RGB (0, 0, 0)
    mappend = (<>)



op :: Integer -> Integer -> Integer 
op a b
    | elem a [0,1] && elem b [0,1] = if (a == b) then 0 else 1
    | otherwise = error "nur 0 oder 1"
instance Semigroup Integer where 
    (<>) = op 
instance Monoid Integer where 
    mempty = 0
    mappend = (<>)   
