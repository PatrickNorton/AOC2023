module Main where

import Control.Monad
import Text.Parsec
import Text.Parsec.String (Parser, parseFromFile)
import Util

import Paths_AOC2023_hs

data Map = Map
  { name :: String
  , vals :: [(Integer, Integer, Integer)] }
  deriving (Show)

data Data = Data
  { seeds :: [Integer]
  , maps :: [Map] }
  deriving (Show)

line :: Parser (Integer, Integer, Integer)
line = do
  i1 <- number
  void $ char ' '
  i2 <- number
  void $ char ' '
  i3 <- number
  void newline
  return (i1, i2, i3)

mapName :: Parser String
mapName = many1 (oneOf ('-':['a'..'z']))

parseMap :: Parser Map
parseMap = do
  n <- mapName
  void $ string " map:"
  void newline
  v <- many1 line
  return $ Map { name = n, vals = v }

parseData :: Parser Data
parseData = do
  void $ string "seeds: "
  s <- number `sepBy` char ' '
  void newline
  void newline
  m <- parseMap `sepBy` newline
  return $ Data { seeds = s, maps = m }

convertOne :: Integer -> Map -> Integer
convertOne i (Map {name = _, vals = []}) = i
convertOne i (Map {name = n, vals = (d, s, c):rest})
  | i >= s && (i - s) < c = d + (i - s)
  | otherwise = convertOne i (Map {name = n, vals = rest})

convertAll :: Integer -> [Map] -> Integer
convertAll = foldl convertOne

pairs :: [a] -> [(a, a)]
pairs [] = []
pairs [_] = undefined
pairs (a:b:rest) = (a, b):pairs rest

mapRange' :: (Integer, Integer) -> [(Integer, Integer, Integer)] -> [(Integer, Integer)]
mapRange' x [] = [x]
mapRange' (rs, rl) ((d, s, l) : ms)
    | rs <= s + l && s < rs + rl = pre ++ curr ++ post
    | otherwise = mapRange' (rs, rl) ms
  where
    pre = if rs < s then mapRange' (rs, s - rs) ms else []
    curr = [(d + max 0 (rs - s), min rl (l - max 0 (rs - s)))]
    post = if s + l < rs + rl then mapRange' (s + l, rs + rl - s - l) ms else []

part1 :: Data -> Integer
part1 (Data {seeds = s, maps = m}) =
  minimum $ map (`convertAll` m) s

part2 :: Data -> Integer
part2 (Data {seeds = s, maps = m}) =
  fst $ minimum $ foldl mapRange'' (pairs s) (map vals m)
  where
    mapRange'' :: [(Integer, Integer)] -> [(Integer, Integer, Integer)] -> [(Integer, Integer)]
    mapRange'' xs ms = concatMap (`mapRange'` ms) xs

main :: IO ()
main = do
  path <- getDataFileName "day5.txt"
  result <- parseFromFile parseData path
  case result of
    Left err -> error $ show err
    Right file -> do
      print $ part1 file
      print $ part2 file
      return ()
