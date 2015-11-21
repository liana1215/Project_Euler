--Project Euler Problem 1
--Sum of all the multiples of 3 or 5 below specified n
m3and5 :: Integer -> Integer
m3and5 n = sum [x | x <- [1..n], x `mod` 3 == 0 || x `mod` 5 == 0]
