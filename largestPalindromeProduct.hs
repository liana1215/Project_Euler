--Project Euler Problem 4
--Largest palindrome product
import Control.Applicative

largestPalindromeProduct        :: Integer -> Integer
largestPalindromeProduct n      =  foldr (max) 0 [ p | p <- (*) <$> numbers <*> numbers, isPalindrome p]
                                   where numbers = [n `div` 10 .. n] 

--Checks if integer is a palindrome
isPalindrome                    :: Integer -> Bool
isPalindrome n 
                                |  xs == reverse xs = True
                                |  otherwise        = False
                                   where xs = intToList n

--Converts an integer to a list of digits
intToList                       :: Integer -> [Integer]
intToList n 
                                |  n < 0      = error "Negative number"
                                |  n == 0     = [0]
                                |  otherwise  = map (\ x -> read [x] :: Integer) (show n)





