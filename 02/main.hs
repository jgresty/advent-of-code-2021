module Main where

move :: (Int, Int) -> String -> (Int, Int)
move (x, y) ('d' : 'o' : 'w' : 'n' : ' ' : m) = (x, y + read m)
move (x, y) ('u' : 'p' : ' ' : m) = (x, y - read m)
move (x, y) ('f' : 'o' : 'r' : 'w' : 'a' : 'r' : 'd' : ' ' : m) = (x + read m, y)

move' :: (Int, Int, Int) -> String -> (Int, Int, Int)
move' (x, y, aim) ('d' : 'o' : 'w' : 'n' : ' ' : m) = (x, y, aim + read m)
move' (x, y, aim) ('u' : 'p' : ' ' : m) = (x, y, aim - read m)
move' (x, y, aim) ('f' : 'o' : 'r' : 'w' : 'a' : 'r' : 'd' : ' ' : m) =
  (\n -> (x + n, y + aim * n, aim)) $ read m

part1 :: [String] -> Int
part1 =
  (\(x, y) -> x * y) . foldl move (0, 0)

part2 :: [String] -> Int
part2 =
  (\(x, y, _) -> x * y) . foldl move' (0, 0, 0)

main :: IO()
main =
  readFile "./input" >>= print . part2 . lines
