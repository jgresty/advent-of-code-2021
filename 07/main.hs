module Main where

import Data.Text (pack, splitOn, unpack)

solve :: (Int -> Int -> Int) -> [Int] -> Int
solve fn xs =
  minimum $ map (fuel fn xs) $ domain xs
  where
    domain xs = [minimum xs .. maximum xs]
    fuel fn xs x = sum $ map (fn x) xs

distance :: Int -> Int -> Int
distance a =
  abs . ((-) a)

part1 :: [Int] -> Int
part1 =
  solve distance

part2 :: [Int] -> Int
part2 =
  solve exp
  where
    exp a b = (distance a b) * (distance a b + 1) `div` 2

parseInput :: String -> [Int]
parseInput =
  map (read . unpack) . splitOn (pack ",") . pack

main :: IO ()
main =
  readFile "./input" >>= print . part2 . parseInput
