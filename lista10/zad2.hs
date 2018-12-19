

data Array a = Arr Int (Array a) (Array a) | El a | Empty

instance Show e => Show (Array e)  where
  show = show . array_to_list

aempty = Empty
asub (Arr _ a b) k =
  case k `mod` 2 of 0 -> asub a (k `quot` 2)
                    1 -> asub b (k `quot` 2)
asub (El a) k = a

aupdate (Arr size a b) k x =
  case k `mod` 2 of 0 -> (Arr size (aupdate a (k `quot` 2) x) b)
                    1 -> (Arr size a (aupdate b (k `quot` 2) x))

aupdate (El a) _ x = El x

size (Arr s _ _) = s
size (El _) = 1
size Empty = 0

arr Empty Empty = Empty
arr a Empty = a
arr Empty b = b
arr a b =
  Arr ((size a) + (size b)) a b
 
ahiext Empty x = El x
ahiext (El b) x = arr (El b) (El x)
ahiext (Arr s a b) x = 
  case s `mod` 2 of 0 -> arr (ahiext a x) b
                    1 -> arr  a (ahiext b x)

ahirem (El _) = Empty
ahirem (Arr s a b) =
  case (s - 1) `mod` 2 of 0 -> arr (ahirem a) b
                          1 -> arr a (ahirem b)

list_to_array' [] = aempty
list_to_array' (x:xs) = ahiext (list_to_array' xs) x

list_to_array l = list_to_array' (reverse l)

array_to_list' Empty = []
array_to_list' x = (asub x ((size x) - 1)):(array_to_list' (ahirem x))

array_to_list = reverse . array_to_list'

