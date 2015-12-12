--Project Euler Problem 7
--10001st Prime Number

--Constructs list of primes and gets the nth prime number
prime                       :: Int ->  Int
prime n                     =  [x | x <- xs, primeTest x] !! (n-1)
                               where xs = [1..]

--Test whether prime or not
primeTest                   :: Int -> Bool
primeTest n 
        | n < 2             =  False
        | otherwise         =  null [x | x <- [2..(n-1)] , n `mod` x == 0]



