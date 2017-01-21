--Project Euler Problem 9
--Special Pythogorean triplet

triplet :: Integer -> Integer
triplet n = head [ a * b * c | a <- [1..1000], b <- [a..(n-a)], c <- [b..(n-b-a)], (a^2 + b^2 == c^2) && (a + b + c == n)]
      


main = print (triplet 1000)
