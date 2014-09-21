{- Lazyness study

   Problem: given a list of integers, read the first line as N. Return the sum of the
   next N lines. Repeat until a 0 is read.

   Version using f loads the entire data into the heap
   while g streams them.

   Tested using:

   ghc -O2 --make test_lazyness.hs -prof -auto-all -caf-all -fforce-recomp -rtsopts
   hp2ps test_lazyness.hp
   ps2pdf test_lazynes.ps
   analyze the graph
-}


main = interact $ unlines . g . lines
-- SLOW!
-- main = interact $ unlines . f . lines

f (l:ls)
  | n == 0    = []
  | otherwise = [show rr] ++ (f rest)
     where n = read l
           (xs, rest) = splitAt n ls
           rr = h xs
f _ = []

g (l:ls)
  | n == 0    = []
  | otherwise = g' n ls 0
     where n = read l
g _ = []

g' n (l:ls) cnt
  | n == 0 = [show cnt] ++ (g (l:ls))
  | otherwise = g' (n-1) ls (cnt +1)

h = length
