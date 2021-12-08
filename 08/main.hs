module Main where

import Data.List
import Data.Maybe
import Data.Set (fromList)
import Data.Text (pack, splitOn, unpack)

type Input = ([String], [String])

genMap :: [String] -> [String]
genMap xs =
  [zero, one, two, three, four, five, six, seven, eight, nine]
  where
    check fn = fromJust $ find fn xs
    hasLength n = (== n) . length
    hasDistance n x = (== n) . (-) (length x) . length . intersect x
    contains x = (== x) . intersect x

    zero = check $ (\x -> hasLength 6 x && x /= six && x /= nine)
    one = check $ hasLength 2
    two = check $ (\x -> hasLength 5 x && x /= five && x /= three)
    three = check $ (\x -> hasLength 5 x && contains one x)
    four = check $ hasLength 4
    five = check $ (\x -> hasLength 5 x && hasDistance 1 six x)
    six = check $ (\x -> hasLength 6 x && not (contains one x))
    seven = check $ hasLength 3
    eight = check $ hasLength 7
    nine = check (\x -> hasLength 6 x && contains four x)

part1 :: [Input] -> Int
part1 =
  length . filter selector . concatMap (map length) . map snd
  where
    is1 = (== 2)
    is4 = (== 4)
    is7 = (== 3)
    is8 = (== 7)
    selector x = is1 x || is4 x || is7 x || is8 x

part2 :: [Input] -> Int
part2 =
  sum . map (toInt . decode)
  where
    toInt = foldl (\v b -> (v * 10) + b) 0
    isEqual x y = fromList x == fromList y
    getNum tst val = fromJust $ findIndex (isEqual val) (genMap tst)
    decode (tst, val) = map (getNum tst) val

parseInput :: String -> Input
parseInput =
  toTuple . map (words . unpack) . splitOn (pack "|") . pack
  where
    toTuple [x, y] = (x, y)

main :: IO ()
main =
  readFile "./input" >>= print . part2 . map parseInput . lines
