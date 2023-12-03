module Main where

import Paths_AOC2023_hs
import Data.List
import Data.Maybe

data Symbol = Plus | Star | Hash | Dollar | Slash | Amp | Percent

data Value = Symbol Char | Num Int | Dot deriving Show

type Line = [Value]

toSym :: Char -> Value
toSym '.' = Dot
toSym '0' = Num 0
toSym '1' = Num 1
toSym '2' = Num 2
toSym '3' = Num 3
toSym '4' = Num 4
toSym '5' = Num 5
toSym '6' = Num 6
toSym '7' = Num 7
toSym '8' = Num 8
toSym '9' = Num 9
toSym c = Symbol c

enumerate :: [a] -> [(Int, a)]
enumerate = zip [0..]

isSymb :: Value -> Bool
isSymb (Symbol _) = True
isSymb _ = False

isStar :: Value -> Bool
isStar (Symbol '*') = True
isStar _ = False

leadingNumber :: String -> (String, String)
leadingNumber (a:rest) | a `elem` ['0'..'9'] =
                         let (n, r) = leadingNumber rest in
                           (a:n, r)
                       | otherwise = ("", a:rest)
leadingNumber "" = ("", "")

innerNos :: Int -> String -> Int -> [(Int, [(Int, Int)])]
innerNos x st y = case leadingNumber st of
  ("", "") -> []
  ("", _:rest) -> innerNos x rest (y+1)
  (s, r) -> let dig = read s in
              let f _ [] = []
                  f y' (_:r') = (x, y'):f (y' + 1) r' in
                (dig, f y s) : innerNos x r (y + length s)

numbersLine :: Int -> String -> [(Int, [(Int, Int)])]
numbersLine x s = innerNos x s 0

numbers :: [String] -> [(Int, [(Int, Int)])]
numbers ls = concatMap (uncurry numbersLine) (enumerate ls)

symbols :: [[Value]] -> [(Int, Int)]
symbols s = concatMap (\(i, x) -> map (\y -> (i, y)) (findIndices isSymb x)) (enumerate s)

stars :: [[Value]] -> [(Int, Int)]
stars s = concatMap (\(i, x) -> map (\y -> (i, y)) (findIndices isStar x)) (enumerate s)

isAdjacent :: (Int, Int) -> (Int, Int) -> Bool
isAdjacent (x, y) (x', y') = abs (x - x') <= 1 && abs (y - y') <= 1

nextToSym :: [(Int, Int)] -> [(Int, Int)] -> Bool
nextToSym nums syms = any (\x -> any (isAdjacent x) syms) nums

part1 :: [String] -> Int
part1 s = let vals = map (map toSym) s in
            let syms = symbols vals
                nums = numbers s in
              sum . map fst $ filter (\(_, v) -> nextToSym v syms) nums

gears :: [(Int, [(Int, Int)])] -> [(Int, Int)] -> [(Int, Int)]
gears nums =
  mapMaybe (\(x, y) -> case filter (\(_, vals) -> any (isAdjacent (x, y)) vals) nums of
             [(a, _), (b, _)] -> Just (a, b)
             _ -> Nothing)

catPairs :: [(a, a)] -> [a]
catPairs [] = []
catPairs ((a, b):rest) = a:b:catPairs rest

part2 :: [String] -> Int
part2 s = let vals = map (map toSym) s in
            let syms = stars vals
                nums = numbers s in
              sum . map (uncurry (*)) $ gears nums syms

main :: IO ()
main = do
  path <- getDataFileName "day3.txt"
  text <- readFile path
  let arr = lines text
  print $ part1 arr
  print $ part2 arr
