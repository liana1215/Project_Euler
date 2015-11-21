--Project Euler Problem 3
--Largest prime factor
largestPrimeFactor :: Integer -> Integer
largestPrimeFactor n = foldl max 0 (primeFactor n (prime n))

--Constructs list of Primes up to a given n
prime :: Integer -> [Integer]
prime n = [x | x <- xs, primeTest x]
    where xs = [1..n]

--Test whether prime or not
primeTest :: Integer -> Bool
primeTest 0 = False
primeTest 1 = False
primeTest n = null [x | x <- [2..(n-1)] , n `mod` x == 0]

--Constructs list of prime factors
primeFactor :: Integer -> [Integer] -> [Integer]
primeFactor n (x:xs)
    | n         == 1 = []
    | n `mod` x == 0 = [x] ++ primeFactor (n `div` x) (x:xs)
    | otherwise      = primeFactor n xs
    





