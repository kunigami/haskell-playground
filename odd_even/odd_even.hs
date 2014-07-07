{-
  From stdin, read a line containing a natural N. Read the next two
  lines with the names of two players. Read the next N lines containing
  n and m representing a odd-or-even game. If player 1 wins, print the name
  of the first player. Otherwise print the name of the second player.
  Repeat until N == 0.
-}

import Data.List

main = interact $ (testCases 1) . lines

-- TODO: how can we use the state monad for a more elegant code here?
testCases::Int -> [String] -> String
testCases t (l:ls)
      | n > 0 = (testCase t (take n ls)) ++ (testCases (t+1) (drop n ls))
      | otherwise = ""
         where n = read l
testCases _ _ = ""

testCase::Int -> [String] -> String
testCase t (player1:player2:scores) =
  (testHeader t) ++ (testBody scores player1 player2) ++ "\n\n"

testHeader::Int -> String
testHeader t = "Test " ++ (show t) ++ "\n"

testBody::[String] -> String -> String -> String
testBody scores player1 player2 =
  intercalate "\n" (map evalMatch scores)
    where evalMatch = \score -> if (player1Won score) then player1 else player2

player1Won::String -> Bool
player1Won score = isEven . parseScore $ score

parseScore::String -> Int
parseScore score = sum tokens
  -- We assume tokens will always have 2 elements
  where tokens = (map read) . words $ score::[Int]

isEven::Int -> Bool
isEven x = x `mod` 2 == 0
