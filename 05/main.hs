module Main where

import qualified Data.Set as Set
import Text.Regex

type Point = (Int, Int)

type Line = (Point, Point)

type State = (Set.Set Point, Set.Set Point)

axisAlign :: Line -> Bool
axisAlign ((x, y), (x', y')) =
  x == x' || y == y'

toPoints :: Line -> [Point]
toPoints ((x, y), (x', y')) =
  case (x' - x, y' - y) of
    (0, 0) -> [(x, y)]
    (i, j) -> (x, y) : toPoints ((x + signum i, y + signum j), (x', y'))

step :: State -> Point -> State
step (found, dupes) point =
  (Set.insert point found, dupes')
  where
    dupes' =
      if Set.member point found
        then Set.insert point dupes
        else dupes

part1 :: [Line] -> Int
part1 =
  part1 . filter axisAlign

part2 :: [Line] -> Int
part2 =
  Set.size . snd . foldl step (Set.empty, Set.empty) . concat . map toPoints'

parseInput :: String -> Line
parseInput str =
  case matchRegex regex str of
    Just [x, y, x', y'] -> ((read x, read y), (read x', read y'))
    Nothing -> ((0, 0), (0, 0))
  where
    regex = mkRegex "([0-9]+),([0-9]+) -> ([0-9]+),([0-9]+)"

main :: IO ()
main =
  readFile "./input" >>= print . part1 . map parseInput . lines
