module Main where

import Control.Monad
import Data.Char
import Data.List
import Data.Maybe

type Board = [[Maybe Int]]

markBoard :: Int -> Board -> Board
markBoard num =
  map $ map (>>= matches)
  where
    matches a =
      if a == num
        then Nothing
        else Just a

winner :: Board -> Maybe Int
winner b =
  if testLine b || testLine (transpose b)
    then Just $ total b
    else Nothing
  where
    testLine = any (all isNothing)
    total = sum . map (sum . catMaybes)

part1 :: [Int] -> [Board] -> Int
part1 (x : xs) boards =
  case msum (map winner newState) of
    Just total -> x * total
    Nothing -> part1 xs newState
  where
    newState = map (markBoard x) boards

part2 :: [Int] -> [Board] -> Int
part2 (x : xs) boards =
  case boards of
    b : [] -> part1 (x : xs) boards
    bs -> part2 xs $ filter (isNothing . winner) $ map (markBoard x) bs

split :: (a -> Bool) -> [a] -> [[a]]
split fn xs = case dropWhile fn xs of
  [] -> []
  xs' -> x : split fn xs''
    where
      (x, xs'') = break fn xs'

parseInput :: [String] -> ([Int], [Board])
parseInput (x : xs) =
  (nums, boards)
  where
    nums = map read $ split (== ',') x
    boards = map (map toLine) $ split (== []) xs
    toLine = map Just . map read . split isSpace

main :: IO ()
main =
  readFile "./input" >>= print . uncurry part2 . parseInput . lines
