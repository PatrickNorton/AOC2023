module Main where

import Control.Monad
import Text.Parsec
import Text.Parsec.String
import Util

import Paths_AOC2023_hs

parseData :: Parser [(Int, Int)]
parseData = do
  void $ string "Time:"
  spaces
  ts <- number `sepBy` many1 (char ' ')
  void newline
  void $ string "Distance:"
  spaces
  vs <- number `sepBy` many1 (char ' ')
  void newline
  eof
  return $ zip ts vs

noSpaceNum :: (Integral a) => (Read a) => Parser a
noSpaceNum = do
  digits <- many1 (digit <|> char ' ')
  let digs = filter (/= ' ') digits
  return $ read digs

parseSpaceless :: Parser (Int, Int)
parseSpaceless = do
  void $ string "Time:"
  spaces
  t <- noSpaceNum
  void newline
  void $ string "Distance:"
  spaces
  v <- noSpaceNum
  void newline
  eof
  return (t, v)

dist :: Int -> Int -> Int
dist len wait = (len - wait) * wait

winCount :: (Int, Int) -> Int
winCount (len, m) = length . filter (>m) $ map (dist len) [0..len]

part1 :: [(Int, Int)] -> Int
part1 = product . map winCount

part2 :: (Int, Int) -> Int
part2 = winCount

main :: IO ()
main = do
  path <- getDataFileName "day6.txt"
  result <- parseFromFile parseData path
  case result of
    Left err -> error $ show err
    Right file -> do
      print $ part1 file
  result' <- parseFromFile parseSpaceless path
  case result' of
    Left err -> error $ show err
    Right file -> do
      print $ part2 file
      return ()
