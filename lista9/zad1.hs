
f (x:xs) = [ i | i <- xs, i `mod` x /= 0 ]

make_primes s@(x:xs) = x:(make_primes (f s))
primes = make_primes [2..]


