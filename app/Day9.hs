module Main where

import Paths_AOC2023_hs

diffs :: [Int] -> [Int]
diffs [a, b] = [b - a]
diffs (a:b:rest) = (b - a):diffs (b:rest)
diffs _ = undefined

extrapolate :: [Int] -> Int
extrapolate x | all (== 0) x = 0
              | otherwise = last x + extrapolate (diffs x)

extBack :: [Int] -> Int
extBack x | all (== 0) x = 0
          | otherwise = head x - extBack (diffs x)

part1 :: [[Int]] -> Int
part1 l = sum $ map extrapolate l

part2 :: [[Int]] -> Int
part2 l = sum $ map extBack l

main :: IO ()
main = do
  path <- getDataFileName "day9.txt"
  f <- readFile path
  let dat = map (map read . words) $ lines f
  print $ part1 dat
  print $ part2 dat
