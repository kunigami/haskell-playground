{-
  How many small boxes fit in a big box?

  Constraint: boxes cannot be rotated to fit, so if the small box
  dimension is (x, y, z) and the larger box is (X, Y, Z), the dimensions
  much match.

  (x, y, z) is given in the first line, (X, Y, Z) in the second
  line. Read from stdin, output a single number, the number of small
  boxes that fit in a big box

-}

main = interact ((++ "\n") . show . solve . lines)

solve::[String] -> Int
solve (l1:l2:_) =  product $ zipWith quot largeBox smallBox
                    where smallBox = parseDim l1
                          largeBox = parseDim l2

parseDim::String -> [Int]
parseDim = (map read) . words
