{-

Problem: Given a value v, and coins of values $50, $10, $5 and $1,
find a way to represent v using those coins in such a way that the
number of coins is minimal.

- State Monad
- List functions: intersperse, concat

-}
import Control.Monad.State.Lazy
import Data.List

main = interact $ unlines . runProgram . lines

runProgram::[String] -> [String]
runProgram ls = fst $ (runState $ runTestCases xs) 1
                 where xs = (takeWhile (/= 0)) . (map read) $ ls

runTestCases::[Int] -> State Int [String]
runTestCases ls = mapM runTestCase ls

runTestCase::Int -> State Int String
runTestCase s = state $ \t -> ((printHeader t) ++ (solve s), t + 1)

printHeader::Int -> String
printHeader t = "Test " ++ (show t) ++ "\n"

solve::Int -> String
solve s = (stringify results) ++ "\n"
           where results   = findChange s [50, 10, 5, 1]
                 stringify = concat . (intersperse " ") . (map show)

findChange::Int -> [Int] -> [Int]
findChange rm (v:values) = count:(findChange newRm values)
            where count = rm `div` v
                  newRm = rm `mod` v
findChange _ [] = []
