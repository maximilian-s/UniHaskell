
-- 10-1 a) Typ Synonyme Money und Account
type Money = Int 
type Account = (Money, Money)


-- 10-1 b) Funktionen withdraw und deposit
withdraw :: Money -> Account -> Maybe Account
withdraw amount (credit, debit) | amount > 0 || abs (amount + debit) > credit = Nothing
                                | otherwise = Just (credit, debit + abs (amount)) 

deposit :: Money -> Account -> Maybe Account
deposit amount (credit, debit) | amount < 0 = Nothing
                               | otherwise =  Just (credit + amount , debit)

-- 10-1 c) Bsp 1 und Bsp 2 in Do Notation
banktag :: Maybe Balance
banktag = do 
    start <- return (0,0)
    first <- deposit 99 start
    accountState first
    second <- withdraw (-2) first
    accountState second
    third <- withdraw (-15) second
    accountState third
    fourth <- deposit 19 third
    accountState fourth 
    fifth <- withdraw (-80) fourth
    accountState fifth

banktag2 :: Maybe Balance
banktag2 = do 
    start <- return (0,0)
    first <- deposit 99 start
    second <- withdraw (-2) first
    third <- withdraw (-150) second
    fourth <- deposit 19 third
    fifth <- withdraw (-80) fourth
    accountState fifth

-- 10-1 d) Bsp 1 und Bsp 2 in >>= Notation

banktag3 :: Maybe Account
banktag3 = return (0,0) >>= deposit 99 >>= withdraw (-2)
            >>= withdraw (-15) >>= deposit 19 >>= withdraw (-80) 


-- 10-1 e) Funktion accountState 

type Balance = Money

accountState :: Account -> Maybe Balance 
accountState (credit, debit) = Just (credit - debit)
--accountState _ = Nothing 



-- Aufgabe 10-2
-- a
data Retrieveable x = NotAvailable | Present x

--b 
instance Functor Retrieveable where
    fmap f NotAvailable = NotAvailable
    fmap f (Present x) = Present (f x)

instance Applicative Retrieveable where
    pure = Present
    _ <*> NotAvailable = NotAvailable
    NotAvailable <*> _ = NotAvailable
    Present f <*> Present x = Present (f x)

instance Monad Retrieveable where 
    return = pure 
    Present x >>= f = f x
    NotAvailable >>= _ = NotAvailable
    