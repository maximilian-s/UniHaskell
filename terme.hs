-- Implementation of simple mathematical terms in Haskell. eval evaluates a given term, simplify simplifies it. Both functions are implemented using standard recursion.

data Operator = Plus | Minus | Times | By deriving Show

data Term a = C a | UnaryTerm Operator (Term a)| BinaryTerm Operator (Term a) (Term a) deriving Show 

eval :: Integral a => Term a -> a
eval (C a) = a
eval (UnaryTerm Minus a) = - (eval a)
eval (BinaryTerm Plus a b) = eval a + eval b
eval (BinaryTerm Minus a b) = eval a - eval b
eval (BinaryTerm Times a b) = eval a * eval b
eval (BinaryTerm By a b) = eval a `div` eval b

simplify :: Integral a => (Term a) -> (Term a)
simplify (C n) = C n
simplify (UnaryTerm Minus (UnaryTerm Minus n)) = simplify n
simplify (BinaryTerm Plus x (UnaryTerm Minus y)) = BinaryTerm Minus (simplify x) (simplify y)
simplify (BinaryTerm Minus x (UnaryTerm Minus y)) = BinaryTerm Plus (simplify x) (simplify y)

