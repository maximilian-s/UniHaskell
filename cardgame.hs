-- Implementation of a custom Data Type Card, including game logic with custom implementation of Ord --

data Suit = Club | Heart | Spade | Diamond deriving (Show, Enum, Eq)
data Value = Two | Three | Four | Five | Six | Seven | Eight | Nine | Ten | Jack | Queen | King | Ace deriving (Show,Eq,Enum,Ord, Bounded)

data Card = Card {suit :: Suit, value :: Value} deriving (Show,Eq)


instance Ord Suit where
    compare Heart Heart = EQ
    compare Heart _ = GT
    compare _ Heart = LT
    compare _ _ = EQ

instance Ord Card where
    compare (Card Heart a) (Card Heart b) = compare a b
    compare (Card Heart _) (Card _ _) = GT
    compare (Card _ _) (Card Heart _) = LT
    compare (Card _ a) (Card _ b) = compare a b

createDeck = [Card {suit = y, value = x}| x <- [Two .. Ace], y <- [Club .. Diamond]]
