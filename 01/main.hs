module Main where

part1 :: [Int] -> Int
part1 =
  sum . map fromEnum . (\xs -> zipWith (<) xs (tail xs))

window :: [Int] -> [Int]
window xs =
  zipWith3 (\a b c -> a + b + c) xs (tail xs) (tail $ tail xs)

part2 :: [Int] -> Int
part2 =
  part1 . window

main :: IO()
main = 
  readFile "./input" >>= print . part2 . map read . lines
