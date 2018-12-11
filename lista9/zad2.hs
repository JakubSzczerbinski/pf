

primes = 2:[ x | x <- [3..], (all (\p -> x `mod` p /= 0) (takeWhile (\p -> p*p <= x) primes))]
