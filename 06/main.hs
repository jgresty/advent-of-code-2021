module Main where

import Data.Text (pack, splitOn, unpack)

simFish :: Int -> [Int] -> Int
simFish days xs =
  sum $ iterate step (initialState xs) !! max 0 days
  where
    initialState xs = [length $ filter (== x) xs | x <- [0 .. 8]]
    step (x0 : x1 : x2 : x3 : x4 : x5 : x6 : x7 : x8 : []) =
      x1 : x2 : x3 : x4 : x5 : x6 : (x7 + x0) : x8 : x0 : []

part1 :: [Int] -> Int
part1 = simFish 80

part2 :: [Int] -> Int
part2 = simFish 256

parseInput :: String -> [Int]
parseInput =
  map (read . unpack) . splitOn (pack ",") . pack

main :: IO ()
main =
  readFile "./input" >>= print . part2 . parseInput
