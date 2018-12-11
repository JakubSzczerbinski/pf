
data Tree a = Node (Tree a) a (Tree a) | Leaf
instance Show e => Show (Tree e)  where
   show (Node lt v rt) = "(" ++ show lt ++ ", " ++ show v ++ ", " ++ show rt ++ ")"
   show Leaf = "Leaf"

data Set a = Fin (Tree a) | Cofin (Tree a)
instance Show a => Show (Set a) where
   show (Fin a) = "Fin " ++ show a
   show (Cofin a) = "Cofin " ++ show a


insert el tree = union tree (Node Leaf el Leaf)

treeFromList (x:xs) = insert x (treeFromList xs)
treeFromList [] = Leaf

union Leaf b = b
union a Leaf = a
union a@(Node lt av rt) b@(Node _ bv _) =
   if av > bv 
   then Node lt av (union rt b)
   else Node (union lt b) av rt

intersection Leaf a = Leaf
intersection b Leaf = Leaf
intersection a@(Node alt av art) b@(Node blt bv brt) =
   case compare bv av of
      EQ -> Node (alt `intersection` blt) av (art `intersection` brt)
      LT -> (a `intersection` blt) `union` (art `intersection` b)
      GT -> (alt `intersection` b) `union` (a `intersection` brt)

diffrence a Leaf = a
diffrence Leaf b = Leaf
diffrence a@(Node alt av art) b@(Node blt bv brt) =
   if av `member` b 
   then (alt `diffrence` b) `union` (art `diffrence` b)
   else Node (alt `diffrence` b) av (art `diffrence` b)

member el (Node lt v rt) =
   case compare el v of
      LT -> member el rt
      EQ -> True
      GT -> member el lt
member el Leaf = False

setFromList a = Fin (treeFromList a)
setEmpty = Fin (Leaf)
setFull = Cofin (Leaf)

setUnion (Fin a) (Fin b) = Fin (a `union` b)
setUnion (Fin a) (Cofin b) = Cofin (b `diffrence` a)
setUnion a b@(Fin _) = setUnion b a
setUnion (Cofin a) (Cofin b) = Cofin (a `intersection` b)

setIntersection (Fin a) (Fin b) = Fin (a `intersection` b)
setIntersection (Fin a) (Cofin b) = Fin (a `diffrence` b)
setIntersection a b@(Fin _) = setIntersection b a
setIntersection (Cofin a) (Cofin b) = Cofin (a `union` b)

setComplement (Fin a) = Cofin a
setComplement (Cofin a) = Fin a

setMember el (Fin a) = member el a
setMember el (Cofin a) = not (member el a)

a = setFromList [1, 2, 3]
b = setFromList [2, 5, 6]
c = setComplement (setFromList [2, 3, 5])
d = setComplement (setFromList [10, 3, 8])
      
