
sublists l@(x:xs) = l:(sublists xs)
sublists [] = []

