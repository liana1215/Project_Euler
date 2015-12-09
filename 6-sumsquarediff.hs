--Project Euler Problem 6
--Sum square difference

import Control.Applicative


sumSquareDiff    :: Int -> Int
sumSquareDiff n  = squareSum n - sumSquare n

sumSquare        :: Int -> Int
sumSquare n      = sum $ (^2) <$> [1..n]

squareSum        :: Int -> Int
squareSum n      = (^2) $ sum [1..n]



