module AOC where
import System.IO ( IOMode (ReadMode), hGetContents, openFile, hClose )
import System.Environment (getArgs)

getInput :: IO String
getInput = do
  test <- getArgs
  let fileName = (if test == ["test"] then "test" else "input") ++ ".txt"
  readFile fileName

getLines :: IO [String]
getLines = lines <$> getInput

split :: Char -> String -> [String]
split c xs
  | c `notElem` xs = [xs]
  | otherwise = chunk:split c (drop (length chunk + 1) xs)
    where chunk = takeWhile (/= c) xs