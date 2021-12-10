import AOC

main = do
  input <- getInput
  print $ partOne input
  print $ partTwo input

partOne :: String -> Int
partOne input = length (filter (=='(') input) - length (filter (==')') input)

moveFloor :: (Int, Int) -> Char -> (Int, Int)
moveFloor (current, idx) command
  | command == '(' = (current+1, idx+1)
  | otherwise = (current-1, idx+1)

partTwo :: String -> Int
partTwo input = snd . head . dropWhile ((>=0) . fst ) $ scanl moveFloor (0, 0) input