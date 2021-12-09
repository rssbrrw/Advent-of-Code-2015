module AOC where
import System.IO ( IOMode (ReadMode), hGetContents, openFile )
import System.Environment (getArgs)

getInput :: IO String
getInput = do
  test <- getArgs
  let fileName = (if test == ["test"] then "test" else "input") ++ ".txt"
  handle <- openFile fileName ReadMode
  hGetContents handle