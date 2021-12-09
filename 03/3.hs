{-# OPTIONS_GHC -fno-warn-incomplete-patterns #-}

import Data.Bifunctor (bimap)
import Data.Map as M

import AOC

main = do
  input <- getInput
  print $ partOne input
  print $ partTwo input

initialState = insert 0 (insert 0 True empty) empty

move :: ((Int, Int), Map Int (Map Int Bool)) -> Char -> ((Int, Int), Map Int (Map Int Bool))
move ((x, y), m) direction =
  case direction of
    '^' -> ((x, y+1), insert x (insert (y+1) True (m!x)) m)
    'v' -> ((x, y-1), insert x (insert (y-1) True (m!x)) m)
    '>' -> ((x+1, y), insert (x+1) (insert y True (if member (x+1) m then m!(x+1) else empty)) m)
    '<' -> ((x-1, y), insert (x-1) (insert y True (if member (x-1) m then m!(x-1) else empty)) m)

partOne :: String -> Int
partOne input = do
  sum $ M.map size $ snd $ Prelude.foldl move ((0, 0), initialState) input

alternateChars :: String -> (String, String)
alternateChars xs = bimap reverse reverse (Prelude.foldl addToShortest ("", "") xs)
  where addToShortest = \(s1, s2) c -> (if length s2 < length s1 then (s1, c:s2) else (c:s1, s2))

partTwo :: String -> Int
partTwo input = do
  let (santaState, roboState) = (initialState, initialState)
  let (santaDirections, roboDirections) = alternateChars input
  let santaRoute = Prelude.foldl move ((0, 0), santaState) santaDirections
  let bothRoutes = snd $ Prelude.foldl move ((0, 0), snd santaRoute) roboDirections
  sum $ M.map size bothRoutes
