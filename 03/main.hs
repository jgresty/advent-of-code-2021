module Main where

import Data.Bits
import Data.List

mode :: [Bool] -> Bool
mode xs =
  (<=) ((fromIntegral $ length xs) / 2) $ fromIntegral $ length $ filter ((==) True) xs

toInt :: [Bool] -> Int
toInt =
  foldl (\v b -> (v `shiftL` 1) + fromEnum b) 0

rating :: ([Bool] -> Bool) -> Int -> [[Bool]] -> Int
rating fn a (x : []) = toInt x
rating fn a xs =
  rating fn (a + 1) $ filter (\y -> (y !! a) == filterBit) xs
  where
    filterBit = fn $ (\b -> b !! a) $ transpose xs

part1 :: [[Bool]] -> Int
part1 xs =
  epsilon * gamma
  where
    gamma = compute mode
    epsilon = compute (not . mode)
    compute fn = toInt $ map fn $ transpose xs

part2 :: [[Bool]] -> Int
part2 xs =
  oxyGen * co2Scrub
  where
    oxyGen = rating mode 0 xs
    co2Scrub = rating (not . mode) 0 xs

parseBits :: String -> [Bool]
parseBits [] = []
parseBits ('0' : xs) = False : parseBits xs
parseBits ('1' : xs) = True : parseBits xs

main :: IO ()
main =
  readFile "./input" >>= print . part2 . map parseBits . lines
