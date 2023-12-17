{-# LANGUAGE TupleSections #-}

module Main where

import Data.Char
import Data.Functor
import qualified Data.Map as M
import Data.Maybe
import Util
import Util.Search

import Paths_AOC2023_hs

type Grid = [[Int]]

data Dir = U | D | L | R deriving (Eq, Ord, Show)
data DState = DState (Int, Int) Dir Int deriving (Eq, Ord, Show)

statePos :: DState -> (Int, Int)
statePos (DState p _ _) = p

deltas :: DState -> [DState]
deltas (DState (r, c) U n) = [ DState (r - 1, c) U (n + 1)
                             , DState (r, c - 1) L 1
                             , DState (r, c + 1) R 1]
deltas (DState (r, c) D n) = [ DState (r + 1, c) D (n + 1),
                               DState (r, c - 1) L 1
                             , DState (r, c + 1) R 1]
deltas (DState (r, c) L n) = [ DState (r, c - 1) L (n + 1)
                             , DState (r - 1, c) U 1
                             , DState (r + 1, c) D 1]
deltas (DState (r, c) R n) = [ DState (r, c + 1) R (n + 1)
                             , DState (r - 1, c) U 1
                             , DState (r + 1, c) D 1]

neighbors :: Grid -> DState -> [(Int, DState)]
neighbors m p
  = mapMaybe (\p'@(DState l _ _) -> index2d m l <&> (,p')) . filter valid $ deltas p
  where
    valid (DState _ _ n) = n <= 3

targetP :: Grid -> DState -> Bool
targetP g = (== target) . statePos
  where target = both (subtract 1) $ bounds g

answer :: Grid -> M.Map DState Int -> M.Map DState DState -> Int
answer grid dist _ = minimum $ M.filterWithKey (\a _ -> targetP grid a) dist

part1 :: Grid -> Int
part1 grid = minimum $ map (\start -> dijkstra start t n a) starts
  where
    starts = [DState (0, 0) D 1, DState (0, 0) R 1]
    t = targetP grid
    n = neighbors grid
    a = answer grid

neighbors' :: Grid -> DState -> [(Int, DState)]
neighbors' m p@(DState _ d n)
  = mapMaybe (\p'@(DState l _ _) -> index2d m l <&> (,p')) . filter valid $ deltas p
  where
    valid (DState _ d' n') = (n >= 4 || d == d') && n' <= 10

targetP' :: Grid -> DState -> Bool
targetP' g = \(DState p _ n) -> n >= 4 && p == b
  where b = bounds g

part2 :: Grid -> Int
part2 grid = minimum $ map (\start -> dijkstra start t n a) starts
  where
    starts = [DState (0, 0) D 1, DState (0, 0) R 1]
    t = targetP' grid
    n = neighbors' grid
    a = answer grid

main :: IO ()
main = do
  path <- getDataFileName "day17.txt"
  grid <- map (map digitToInt) . lines <$> readFile path
  print $ part1 grid
  print $ part2 grid
