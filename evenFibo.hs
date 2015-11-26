--Project Euler Problem 2
--Even Fibonacci numbers
--Calculates the sum of the even-valued terms of the Fibonacci sequence whose values do not exceed n.
evenFibo        :: Integer -> Integer
evenFibo n      =  sum [x | x <- takeWhile (<n) (map (\ y -> fibo y) [1..]), even x]

fibo            :: Integer -> Integer
fibo 0          =  1 
fibo 1          =  1
fibo n          =  fibo (n-1) + fibo (n-2)




