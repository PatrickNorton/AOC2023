module Main where

import Data.List
import Util

import Paths_AOC2023_hs

splitFields :: [[a]] -> [[[a]]]
splitFields [] = []
splitFields (a:[]:r) = [a]:splitFields r
splitFields (a:r) = case splitFields r of
  [] -> [[a]]
  a':r' -> (a:a'):r'

eqToLen :: (Eq a) => [a] -> [a] -> Bool
eqToLen [] _ = True
eqToLen _ [] = True
eqToLen (a:ar) (b:br) = a == b && eqToLen ar br

reflections :: (Eq a) => [a] -> [Int]
reflections l = filter (\i -> reverse (take i l) `eqToLen` drop i l) (ind l)

reflectVert :: (Eq a) => [[a]] -> [Int]
reflectVert = reflections . transpose

reflectHoriz :: (Eq a) => [[a]] -> [Int]
reflectHoriz = reflections

reflectScore :: [Int] -> [Int] -> Int
reflectScore v h = sum v + 100 * sum h

offByOne :: (Eq a) => [a] -> [a] -> Bool
offByOne [] _ = False
offByOne _ [] = False
offByOne (a:ar) (b:br) | a == b = offByOne ar br
                       | otherwise = ar == br

differences :: (Eq a) => [a] -> [a] -> Int
differences [] [] = 0
differences (a:ar) (b:br) | a /= b = 1 + differences ar br
                          | otherwise = differences ar br
differences _ _ = undefined

smudgeHoriz :: (Eq a) => [[a]] -> [Int]
smudgeHoriz l = filter
  (\i -> 1 == sum (zipWith differences (reverse (take i l)) (drop i l)))
  (ind l)

smudgeVert :: (Eq a) => [[a]] -> [Int]
smudgeVert = smudgeHoriz . transpose

part1 :: [[String]] -> Int
part1 = sum . map (\x -> reflectScore (reflectVert x) (reflectHoriz x))

part2 :: [[String]] -> Int
part2 = sum . map (\x -> reflectScore (smudgeVert x) (smudgeHoriz x))

main :: IO ()
main = do
  path <- getDataFileName "day13.txt"
  dat <- splitFields . lines <$> readFile path
  print $ part1 dat
  print $ part2 dat
