
import Data.Maybe

data BTree a = Leaf | Node (BTree a) a (BTree a)

instance Show e => Show (BTree e)  where
   show (Node Leaf v Leaf) = show v
   show (Node lt v rt) = "(" ++ show lt ++ ", " ++ show v ++ ", " ++ show rt ++ ")"
   show Leaf = "()"

dfnum' num Leaf = (Leaf, num)
dfnum' num (Node l v r) = 
  let (lt, num') = dfnum' (num + 1) l in
  let (rt, num'') = dfnum' num' r in
  ((Node lt num rt), num'')

dfnum = fst . (dfnum' 1)

is_parent Leaf = False
is_parent (Node {}) = True

number_trees num ((Node {}):xs) = let (t, num') = number_trees (num+1) xs in ((Just num):t, num')
number_trees num (Leaf:xs) = let (t, num') = number_trees num xs in (Nothing:t, num')
number_trees num [] = ([], num)

children nodes = concatMap aux nodes
  where
    aux Leaf = []
    aux (Node l _ r) = [l, r]
 
bfnum' _ [] = []
bfnum' num nodes =
  let (numbered_nodes, num') = number_trees num nodes in
  let numbered_children = bfnum' num' (children nodes) in 
  aux numbered_children numbered_nodes
  where
    aux [] [] = [] 
    aux cs (Nothing:ps) = Leaf:(aux cs ps)
    aux (cl:(cr:cs)) ((Just n):ps) =  (Node cl n cr):(aux cs ps)


bfnum n = head (bfnum' 1 [n])
  
 
      

