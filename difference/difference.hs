{-
  Read two lines from stdin containing positive integers N and M, and output N-M
-}
main = interact $ (++ "\n") . show . diff . (map read) . lines
         where diff (x0:x1:_) = x0 - x1
