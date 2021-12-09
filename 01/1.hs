import AOC

main = do
  input <- getInput
  print $ partOne input
  print $ partTwo input

partOne :: String -> Int
partOne input = length (filter (=='(') input) - length (filter (==')') input)

moveFloor :: (Int, Int) -> Char -> (Int, Int)
moveFloor (current, idx) command
  | current == -1 = (current, idx)
  | command == '(' = (current+1, idx+1)
  | otherwise = (current-1, idx)

partTwo :: String -> Int
partTwo input = snd $ foldl moveFloor (0, 0) input