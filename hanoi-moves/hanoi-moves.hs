{-
  Print the number of moves it is required for a hanoi tower of N disks.

  The input is given in stdin, one integer at each line, representing N.
  For each input, output to stdout the number of moves necessary, using the format:

  Test <t>
  <number of moves>

  In this problem, I'm exercising the use of the State monad (requires GHC +7).
-}

import Control.Monad.State

main = interact $ unlines . runProgram . lines

runTestCase::Int -> State Int String
runTestCase s = state $ \t -> ((printHeader t) ++ (solve s), t + 1)

runTestCases::[Int] -> State Int [String]
runTestCases ls = mapM runTestCase ls

runProgram::[String] -> [String]
runProgram ls = fst $ (runState $ runTestCases xs) 1
                 where xs = (takeWhile (/= 0)) . (map read) $ ls

printHeader::Int -> String
printHeader t = "Test " ++ (show t) ++ "\n"

solve::Int -> String
solve s = (show $ (2^s-1)) ++ "\n"
