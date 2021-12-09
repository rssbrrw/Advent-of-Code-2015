{-# OPTIONS_GHC -fno-warn-incomplete-patterns #-}

import AOC ( getInput, getLines, split )
import Data.List (sort)

main = do
  input <- map (map (read :: String -> Int) . split 'x') <$> getLines
  print $ partOne input
  print $ partTwo input

paperNeeded :: [Int] -> Int
paperNeeded (x:y:z:_) = 2*x*y + 2*x*z + 2*y*z + smallest
  where smallest = product $ take 2 $ sort [x,y,z]

partOne :: [[Int]] -> Int
partOne input = sum $ map paperNeeded input

ribbonNeeded :: [Int] -> Int 
ribbonNeeded dimensions = 2*x + 2*y + x*y*z
  where (x:y:z:_) = sort dimensions

partTwo :: [[Int]] -> Int 
partTwo input = sum $ map ribbonNeeded input
