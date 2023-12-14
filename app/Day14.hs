module Main where

import Data.List
import Util

import Paths_AOC2023_hs

slideRight :: String -> String
slideRight [] = []
slideRight [a] = [a]
slideRight ('O':'#':rest) = 'O':'#':slideRight rest
slideRight ('O':rest) = case slideRight rest of
  [] -> ['O']
  ('O':rest') -> 'O':'O':rest'
  (a:rest') -> a:slideRight('O':rest')
slideRight (a:rest) = a:slideRight rest

slideDown :: [String] -> [String]
slideDown = transpose . map slideRight . transpose

slideUp :: [String] -> [String]
slideUp = reverse . slideDown . reverse

slideLeft :: String -> String
slideLeft = reverse . slideRight . reverse

cyc :: [String] -> [String]
cyc = map slideRight . slideDown . map slideLeft . slideUp

northLoad :: [String] -> Int
northLoad ss = fst $ help ss
  where help [] = (0, 1)
        help (a:rest) = let (s, r) = help rest in
          (s + r * length (filter (=='O') a), r + 1)

cycleIndex :: (Eq a) => Int -> [a] -> a
cycleIndex i = help 0 []
  where help _ _ [] = undefined
        help ci seen (a:rest) | ci == i = a
                              | otherwise = case a `elemIndex` reverse seen of
                                  Just i' -> let cy = drop i' (reverse seen) in
                                    cyI cy (i - ci)
                                  Nothing -> help (ci+1) (a:seen) rest

cyI :: [a] -> Int -> a
cyI a i = a !! (i `mod` length a)

part1 :: [String] -> Int
part1 = northLoad . slideUp

part2 :: [String] -> Int
part2 d = northLoad $ cycleIndex 1000000000 (iterate cyc d)

main :: IO ()
main = do
  path <- getDataFileName "day14.txt"
  dat <- readLines path
  print $ part1 dat
  print $ part2 dat
