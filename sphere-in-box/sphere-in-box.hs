{-

  Does a sphere fit in a box?

  Given two lines, in the first line we are give then radius of the sphere and
  in the second line the dimensions of a box. Output Y if the sphere fits in the box
  or N otherwise.

-}

main = interact (solve . lines)

solve::[String] -> String
solve (l1:l2:_) = if sphereFitsInBox sphereRadius boxDim
                    then "Y\n"
                    else  "N\n"
                    where boxDim = (map read) . words $ l2
                          sphereRadius = read l1

sphereFitsInBox::Int -> [Int] -> Bool
sphereFitsInBox sphereRadius boxDim = minimum boxDim >= sphereRadius
