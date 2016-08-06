
import Rotate

import Primes


sq :: Integer -> Integer
sq = (^ (2 :: Int))

gen :: [Pythagoras]
gen = [ Pythagoras (a, b, c)
      | c <- primes
      , b <- [1 .. c - 1]
      , a <- [1 .. b - 1]
      , sq a + sq b == sq c
      ]

main :: IO ()
main = print $ take 100 gen
