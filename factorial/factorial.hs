{-
   Read from stdin. Given an integer N, output the factorial of N in stdout.
-}

main = do interact $ (++ "\n") . show . factorial
           where factorial x = product[1..(read x)]
