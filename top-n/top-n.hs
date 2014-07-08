{-

  Read from stdin. Given n and m in the first line, read the next n
  lines each containing an integer and print the top m, one in each
  line, in the stdout.

-}

import List(sortBy)

main = interact (unlines . (map show) . solve . lines)

solve::[String] -> [Int]
solve (l1:ls) = selectTopN n (map read ls)
          where n = read . last . words $ l1

selectTopN::Int -> [Int] -> [Int]
selectTopN n ls = (take n) . (sortBy reverseCmp) $ ls

reverseCmp x y
  | x < y = GT
  | x > y = LT
  | otherwise = EQ
