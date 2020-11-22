import Data.List 

-- 8-1 Binärbaum
data BB a = L | K a (BB a) (BB a) deriving (Show)



-- 8-1 a) Liste [5, 7, 12, 3, 1, 9] als Suchbaum darstellen
myTree = K 5 (K 3 (K 1 L L) L) (K 9 (K 7 L L) (K 12 L L))



-- 8-1 b) ist ein binärer Suchbaum
minValbinTree :: Ord a => BB a -> a 
minValbinTree L = error "Empty Tree"
minValbinTree (K w L L) = w 
minValbinTree (K w l r) = minValbinTree l 

maxValbinTree :: Ord a => BB a -> a 
maxValbinTree L = error "Empty Tree"
maxValbinTree (K w L L) = w
maxValbinTree (K w l r) = maxValbinTree r 

maxVal :: Ord a => BB a -> a
maxVal L = error "Empty Tree"
maxVal (K w L L) = w
maxVal (K w L r) = max w (maxVal r)
maxVal (K w l L) = max w (maxVal l)
maxVal (K w l r) = max w $ max (maxVal l) (maxVal r)

minVal :: Ord a => BB a -> a
minVal L = error "Empty Tree"
minVal (K w L L) = w
minVal (K w L r) = min w (minVal r)
minVal (K w l L) = min w (minVal l)
minVal (K w l r) = min w $ min (minVal l) (minVal r)

isBinarySearchTree :: Ord a => BB a -> Bool
isBinarySearchTree L = True
isBinarySearchTree (K w L L) = True 
isBinarySearchTree (K w L r) = w < minVal r && isBinarySearchTree r
isBinarySearchTree (K w l L) = w > maxVal l && isBinarySearchTree l
isBinarySearchTree (K w l r) = w < minVal r && w > maxVal l && isBinarySearchTree l && isBinarySearchTree r

-- 8-1 c) Tiefe eines Suchbaums
depth :: (Num a, Ord a) => BB t -> a
depth (K w L L) = 1
depth (K w l L) = 1 + depth (l)
depth (K w L r) = 1 + depth (r)
depth (K w l r) = 1 + max (depth l) (depth r)



-- 8-1 d) Element in binären Suchbaum einfügen
insert' :: Ord a => a -> BB a -> BB a
insert' n L = K n L L
insert' n (K w l r)
    | n == w = (K n l r)
    | n < w = K w (insert' n l) r
    | n > w = K w l (insert' n r)


-- 8-1 e) aus einer Liste von Werten einen binären Suchbaum erstellen
buildTree :: Ord a => [a] -> BB a
buildTree = foldr insert' L 



-- 8-1 f) zwei binäre Suchbäume vergleichen
instance (Eq a, Ord a) => Eq (BB a) where
    t1 == t2 = isBinarySearchTree t1 && isBinarySearchTree t2 && equalValues (treetoList t1) (treetoList t2)

treeElem :: Eq a => a -> BB a -> Bool
treeElem n L = False 
treeElem n (K w l r) = n == w || treeElem n l || treeElem n r

treetoList :: BB a -> [a]
treetoList L = []
treetoList (K w l r) = [w] ++ treetoList l ++ treetoList r

equalValues :: Eq a => [a] -> [a] -> Bool
equalValues [] [] = True
equalValues [] ys = False 
equalValues (x:xs) ys = (elem x ys) && equalValues xs (delete x ys)


