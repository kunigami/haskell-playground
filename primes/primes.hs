{-
  Read a line from input representing a positive integer. Print "yes"
  is it's prime or "no" otherwise to stdout.
-}

{-
  Naive approach for determining primality by successive divisions. We
  do a boundary check that makes this O(sqr N) instead of O(N).
-}

main = do interact $ isPrimeMessage . abs . read

isPrimeMessage x = (if isPrime x then "yes" else "no") ++ "\n"

isPrime x = x `isDivisibleByGreaterThan` 2

-- Whether x is divisible by any number greater than div
isDivisibleByGreaterThan x div
          | x < 2 = False
          | div*div > x = True
          | x `mod` div == 0 = False
          | otherwise = x `isDivisibleByGreaterThan` (div + 1)
