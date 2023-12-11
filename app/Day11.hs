{-# LANGUAGE TupleSections #-}

module Main where

import Data.List
import Util

import Paths_AOC2023_hs

emptyRows :: [String] -> [Int]
emptyRows = findIndices (all (=='.'))

emptyCols :: [String] -> [Int]
emptyCols = emptyRows . transpose

galaxies :: [String] -> [(Int, Int)]
galaxies s = concatMap (\(i,x) -> map (i,) (elemIndices '#' x)) (enumerate s)

between :: Int -> Int -> Int -> Bool
between x y z | x <= y && y <= z = True
              | z <= y && y <= x = True
              | otherwise = False

expandedDistance :: Int -> [Int] -> [Int] -> (Int, Int) -> (Int, Int) -> Int
expandedDistance fact ex ey (x, y) (x', y') =
  let btwnX = countOf (\v -> between x v x') ex
      btwnY = countOf (\v -> between y v y') ey in
    taxicab (x, y) (x', y') + (btwnX + btwnY) * (fact - 1)

galaxyDistance :: Int -> [String] -> Int
galaxyDistance fact u =
  let ex = emptyRows u
      ey = emptyCols u
      g = galaxies u in
    sum (map (uncurry $ expandedDistance fact ex ey) (cartProd g g)) `div` 2

part1 :: [String] -> Int
part1 = galaxyDistance 2

part2 :: [String] -> Int
part2 = galaxyDistance 1000000

main :: IO ()
main = do
  path <- getDataFileName "day11.txt"
  dat <- lines <$> readFile path
  print $ part1 dat
  print $ part2 dat
