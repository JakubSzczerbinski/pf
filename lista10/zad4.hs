
data Colour = Red | Black
data RBTree a = RBNode Colour (RBTree a) a (RBTree a) | RBLeaf

show_in_colour colour v = 
  case colour of 
    Red -> "\x1b[91m" ++ show v ++ "\x1b[0m"
    Black -> "\x1b[94m" ++ show v ++ "\x1b[0m"
  
instance Show a => Show (RBTree a) where
  show RBLeaf = "."
  show (RBNode colour a x b) = "(" ++ show a ++ " " ++ show_in_colour colour x ++ " " ++ show b ++ ")"

make_rb Black (RBNode Red (RBNode Red a x b) y c) z d =
    RBNode Red (RBNode Black a x b) y (RBNode Black c z d)

make_rb Black (RBNode Red a x (RBNode Red b y c)) z d =
    RBNode Red (RBNode Black a x b) y (RBNode Black c z d)

make_rb Black a x (RBNode Red (RBNode Red b y c) z d) =
    RBNode Red (RBNode Black a x b) y (RBNode Black c z d)

make_rb Black a x (RBNode Red b y (RBNode Red c z d)) =
    RBNode Red (RBNode Black a x b) y (RBNode Black c z d)

make_rb Black a x b = RBNode Black a x b

make_rb Red a x b = RBNode Red a x b


rbempty = RBLeaf

rbinsert' RBLeaf v = make_rb Red RBLeaf v RBLeaf
rbinsert' (RBNode colour a x b) v =
  case v `compare` x of
    LT -> make_rb colour (rbinsert' a v) x b
    EQ -> RBNode colour a x b
    GT -> make_rb colour a x (rbinsert' b v)

rbinsert RBLeaf v = RBNode Black RBLeaf v RBLeaf
rbinsert t v =
  let (RBNode _ a x b) = rbinsert' t v in
  RBNode Black a x b

rb_fromlist [] = RBLeaf
rb_fromlist (x:xs) = rbinsert (rb_fromlist xs) x

split' (x1:x2:xs) (y:ys) =
  let (ps, rs) = split' xs ys in
  (y:ps, rs)
split' [x] ys = ([], ys)
split' [] ys = ([], ys)

split l = split' l l

rbTreeFromList' l = 
  case split l of
  (xs, y:ys) -> 
    let (rt, rh) = rbTreeFromList' xs in
    let (lt, lh) = rbTreeFromList' ys in
    if rh > lh then
      let (RBNode Black a x b) = rt in
      ((make_rb Black lt y (RBNode Red a x b)), lh + 1)
    else
      ((make_rb Black lt y rt), lh + 1)
  ([], []) -> (RBLeaf, 0)

rbTreeFromList = fst . rbTreeFromList'
