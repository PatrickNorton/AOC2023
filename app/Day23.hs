module Main where

import Data.List
import Data.Map ((!))
import qualified Data.Map as M
import Data.Maybe
import qualified Data.Set as S
import Util

import Paths_AOC2023_hs

type WGraph = M.Map (Int, Int) [(Int, (Int, Int))]

findStart :: [String] -> (Int, Int)
findStart g = (0, fromJust . elemIndex '.' $ head g)

findEnd :: [String] -> (Int, Int)
findEnd g = (length g - 1, fromJust . elemIndex '.' $ last g)

isNotWall :: [String] -> (Int, Int) -> Bool
isNotWall g p = case index2d g p of
  Nothing -> False
  Just '#' -> False
  _ -> True

neighbors :: Bool -> [String] -> (Int, Int) -> [(Int, Int)]
neighbors True grid (x, y) = case index2d grid (x, y) of
  Nothing -> undefined
  Just '>' -> [(x, y+1)]
  Just '<' -> [(x, y-1)]
  Just 'v' -> [(x+1, y)]
  Just '^' -> [(x-1, y)]
  _ -> [(x - 1, y), (x + 1, y), (x, y - 1), (x, y + 1)]
neighbors False _ (x, y) = [(x - 1, y), (x + 1, y), (x, y - 1), (x, y + 1)]

getNeighbors :: Bool -> (Int, Int) -> [String] -> [(Int, Int)]
getNeighbors isSlippy pos grid = filter (isNotWall grid) $ neighbors isSlippy grid pos

makeJunctionGraph :: Bool -> [String] -> WGraph
makeJunctionGraph isSlippy grid = treatJunctions (S.singleton start) M.empty [start]
    where start = findStart grid
          end = findEnd grid
          isJunction pos | pos == start || pos == end = True
                         | otherwise = length (getNeighbors isSlippy pos grid) > 2
          treatJunctions _ graph [] = graph
          treatJunctions seen graph (x:queue) = treatJunctions seen' graph' queue'
            where nextJunctions = findNextJunctions (S.singleton x) [(0, x)]
                  graph' = M.insert x nextJunctions graph
                  junctions = filter (`S.notMember` seen) $ map snd nextJunctions
                  seen' = foldr S.insert seen junctions
                  queue' = queue ++ junctions
          findNextJunctions _ [] = []
          findNextJunctions seen ((d, x):queue)
            | d > 0 && isJunction x = (d, x) : findNextJunctions seen queue
            | otherwise = findNextJunctions seen' queue'
            where neighbours = filter (`S.notMember` seen) $ getNeighbors isSlippy x grid
                  seen'      = foldr S.insert seen neighbours
                  queue'     = queue ++ zip (repeat (d + 1)) neighbours

findLongestPath :: Bool -> [String] -> Int
findLongestPath isSlippy input = go S.empty 0 start
    where start = findStart input
          end = findEnd input
          graph = makeJunctionGraph isSlippy input
          go seen pathLen cur
            | cur == end = pathLen
            | otherwise = best
            where seen' = S.insert cur seen
                  neighbours = [(dist, pos) | (dist, pos) <- graph ! cur, pos `S.notMember` seen]
                  best = maximum (0 : map (uncurry (go seen' . (pathLen +))) neighbours)

part1 :: [String] -> Int
part1 = findLongestPath True

part2 :: [String] -> Int
part2 = findLongestPath False

main :: IO ()
main = do
  path <- getDataFileName "day23.txt"
  input <- lines <$> readFile path
  print $ part1 input
  print $ part2 input
