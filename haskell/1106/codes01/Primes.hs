module Primes (
    primes, primal, nextprime, factorize
) where

primes :: [Integer]
primes = sieve [2..] where
    sieve (x:xs) = x : sieve [ y | y <- xs, rem y x /= 0 ]

primal p = p `elem` takeWhile (p >=) primes

nextprime n = head (dropWhile (n >=) primes)

factorize n = factorize' n primes where
    factorize' k (p:ps)
        | k <= 1        = []
        | rem k p == 0  = p : factorize' (quot k p) (p:ps)
        | otherwise     = factorize' k ps
