module Main where

import Control.Monad
import Data.Bits
import Data.List
import Text.Parsec
import Text.Parsec.String

import Paths_AOC2023_hs

newtype Hand = Hand ([Integer], [Integer])

ifilter :: (Int -> a -> Bool) -> [a] -> [a]
ifilter f = map snd . filter (uncurry f) . zip [0..]

number :: (Integral a) => (Read a) => Parser a
number = do
  digits <- many1 digit
  return $ read digits

spcs :: Parser ()
spcs = skipMany $ char ' '

line :: Parser Hand
line = do
  void $ string "Card"
  void spcs
  void number
  void $ string ":"
  void spcs
  hand <- number `sepBy` spcs
  void $ string ".|."
  spcs
  win <- number `sepBy` spcs
  void newline
  return $ Hand (hand, win)

parseFile :: Parser [Hand]
parseFile = many1 line

handCount :: Hand -> Int
handCount (Hand (hand, win)) = length (win `intersect` hand)

handScore :: Hand -> Int
handScore hand = 1 `shift` (handCount hand - 1)

part1 :: [Hand] -> Int
part1 = sum . map handScore

mapN :: (a -> a) -> [a] -> Int -> [a]
mapN _ [] _ = []
mapN _ l 0 = l
mapN f (a:rest) c = f a : mapN f rest (c - 1)

yield :: [(Int, Int)] -> Int
yield mcs = 1 + (sum . map snd $ ifilter (\i mc -> fst mc - i > 0) mcs)

handCounts :: [Int] -> [(Int, Int)]
handCounts = foldl (\cs m -> (m, yield cs) : cs) []

part2 :: [Hand] -> Int
part2 hs = sum . map snd . handCounts $ map handCount hs

main :: IO ()
main = do
  path <- getDataFileName "day4.txt"
  result <- parseFromFile parseFile path
  case result of
    Left err -> error $ show err
    Right file -> do
      print $ part1 file
      print $ part2 file
      return ()
