{-# LANGUAGE TupleSections #-}

module Main where

import Data.Foldable
import Data.Maybe
import Util

import Paths_AOC2023_hs

data Direction = North | South | East | West

divCeil :: (Integral a) => a -> a -> a
divCeil a b = (a `div` b) + (if (a `mod` b) /= 0 then 1 else 0)

tryIndex :: (Eq a) => a -> [a] -> Maybe Int
tryIndex _ [] = Nothing
tryIndex v (a:rest) | v == a = Just 0
                    | otherwise = (1+) <$> tryIndex v rest

elemIndex2d :: (Eq a) => a -> [[a]] -> (Int, Int)
elemIndex2d v ls = fromJust . asum $ map (\(i, l) -> (i,) <$> tryIndex v l) (enumerate ls)

calcTo :: Direction -> (Int, Int) -> (Direction, (Int, Int))
calcTo d (x, y) = (opposite d, case d of
                       North -> (x - 1, y)
                       South -> (x + 1, y)
                       East -> (x, y + 1)
                       West -> (x, y - 1))

opposite :: Direction -> Direction
opposite North = South
opposite South = North
opposite East = West
opposite West = East

next :: (Maybe Direction, Maybe Direction, Maybe Direction, Maybe Direction)
  -> Direction -> (Int, Int) -> (Direction, (Int, Int))
next (n, s, e, w) d (x, y) = case d of
  North -> calcTo (fromJust n) (x, y)
  South -> calcTo (fromJust s) (x, y)
  East -> calcTo (fromJust e) (x, y)
  West -> calcTo (fromJust w) (x, y)

nextPipe :: Direction -> (Int, Int) -> (Direction, (Int, Int))
nextPipe = next (Just South, Just North, Nothing, Nothing)

nextDash :: Direction -> (Int, Int) -> (Direction, (Int, Int))
nextDash = next (Nothing, Nothing, Just West, Just East)

nextL :: Direction -> (Int, Int) -> (Direction, (Int, Int))
nextL = next (Just East, Nothing, Just North, Nothing)

nextJ :: Direction -> (Int, Int) -> (Direction, (Int, Int))
nextJ = next (Just West, Nothing, Nothing, Just North)

next7 :: Direction -> (Int, Int) -> (Direction, (Int, Int))
next7 = next (Nothing, Just West, Nothing, Just South)

nextF :: Direction -> (Int, Int) -> (Direction, (Int, Int))
nextF = next (Nothing, Just East, Just South, Nothing)

nextPos :: Direction -> (Int, Int) -> [String] -> (Direction, (Int, Int))
nextPos d (x, y) board = case (board !! x) !! y of
  '|' -> nextPipe d (x, y)
  '-' -> nextDash d (x, y)
  'L' -> nextL d (x, y)
  'J' -> nextJ d (x, y)
  '7' -> next7 d (x, y)
  'F' -> nextF d (x, y)
  '.' -> error ("Dot found at" ++ show (x, y))
  _ -> undefined

loopSize :: Char -> Direction -> (Int, Int) -> [String] -> Int
loopSize c d (x, y) board =
  let bc = (board !! x) !! y in
    if bc == c then 0 else 1 + (uncurry $ loopSize c) (nextPos d (x, y) board) board

loopPoints :: Char -> Direction -> (Int, Int) -> [String] -> [(Int, Int)]
loopPoints c d (x, y) board =
  let bc = (board !! x) !! y in
    if bc == c then [(x, y)] else (x, y) : (uncurry $ loopPoints c) (nextPos d (x, y) board) board

shoeLace :: [(Int, Int)] -> Int
shoeLace [_] = 0
shoeLace ((x1, y1) : (x2, y2) : xs) = (y1 + y2) * (x2 - x1) + shoeLace ((x2, y2) : xs)
shoeLace _ = undefined

part1 :: [String] -> Int
part1 s = loopSize 'S' West (snd . calcTo East $ elemIndex2d 'S' s) s `divCeil` 2

part2 :: [String] -> Int
part2 s = let points = loopPoints 'S' West (snd . calcTo East $ elemIndex2d 'S' s) s in
  (abs (shoeLace points) - length points + 3) `div` 2

main :: IO ()
main = do
  path <- getDataFileName "day10.txt"
  dat <- lines <$> readFile path
  print $ part1 dat
  print $ part2 dat
