{-# LANGUAGE TupleSections #-}

module Main where

import Data.List
import Data.Maybe
import qualified Data.Set as S
import Util

import Paths_AOC2023_hs

add2 :: (Num a) => (a, a) -> (a, a) -> (a, a)
add2 (a1, a2) (b1, b2) = (a1 + b1, a2 + b2)

orthos :: [(Int, Int)]
orthos = [(1, 0), (0, 1), (-1, 0), (0, -1)]

orthoNeighbors :: (Int, Int) -> [(Int, Int)]
orthoNeighbors x = map (add2 x) orthos

startPos :: [String] -> (Int, Int)
startPos = head . mapMaybe (\(i, x) -> (i,) <$> ('S' `elemIndex` x)) . enumerate

reachInSteps :: [String] -> Int -> S.Set (Int, Int)
reachInSteps input = ((S.singleton (startPos input) : map inner [1 ..]) !!)
  where
    inner :: Int -> S.Set (Int, Int)
    inner i =
        let prev = reachInSteps input (i - 1)
         in S.fromList
                $ concatMap
                    (\x -> filter (\v -> wrapIndex2d input v /= '#') orthos)
                $ S.toList prev

interpQuad :: [String] -> Int -> Int
interpQuad vs 0 = S.size $ reachInSteps vs 65
interpQuad vs 1 = S.size $ reachInSteps vs (65+length vs)
interpQuad vs 2 = S.size $ reachInSteps vs (65+2*length vs)
interpQuad vs n =
  let a0 = interpQuad vs 0
      a1 = interpQuad vs 1
      a2 = interpQuad vs 2
      b0 = a0
      b1 = a1 - a0
      b2 = a2 - a1 in
    b0 + b1 * n + (n*(n-1) `div` 2)*(b2-b1)

part1 :: [String] -> Int
part1 = S.size . flip reachInSteps 64

part2 :: [String] -> Int
part2 vs = interpQuad vs (26501365 `div` length vs)

main :: IO ()
main = do
  path <- getDataFileName "day21.txt"
  dat <- lines <$> readFile path
  print $ part1 dat
  print $ part2 dat
