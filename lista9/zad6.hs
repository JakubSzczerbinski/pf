
(><) (x:xs) l = (map (\y -> (x, y)) l) ++ (xs >< l)
(><) [] l = []

