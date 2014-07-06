{-
  Read from stdin. The first line is N, the number of following
  lines. Each of the next N lines contain an integer. Output the sum of the N integers
-}

{-
   * words: Reads n lines of strings.
   * tail: Ignores the first one (which contains the number of next lines).
   * (map read): convert list of strings to list of ints
   * sum
   * show: convert int to string
   * (++ "\n"): add new line to the output
-}
main = interact (++ "\n") . show . sum . (map read) . tail . words
