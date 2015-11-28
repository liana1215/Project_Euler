--Project Euler Problem 5
--Smallest multiple
smallestMultiple    :: Integer
smallestMultiple    =  head [x | x <- [1..], not $ divisible x]

divisible           :: Integer -> Bool
divisible n         =  any (>0) $ map (n `mod`) [1..20] 



        
