{-
    Reads from stdin, compute the square and outputs to stdout
 -}

main = interact ((++ "\n") . show . (\x -> x*x) . read)
