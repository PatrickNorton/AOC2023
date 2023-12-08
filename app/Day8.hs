module Main where

import Control.Monad
import Data.List
import Data.Maybe
import Text.Parsec
import Text.Parsec.String

import Paths_AOC2023_hs

type Map = [(String, (String, String))]

newtype Network = Network (String, Map)

parseData :: Parser Network
parseData = do
  dirs <- many1 $ oneOf "LR"
  void newline
  void newline
  lins <- many1 line
  return $ Network (cycle dirs, lins)

line :: Parser (String, (String, String))
line = do
  val <- count 3 upper
  void $ string " = "
  void $ string "("
  left <- count 3 upper
  void $ string ", "
  right <- count 3 upper
  void $ string ")"
  void newline
  return (val, (left, right))

pl :: (String -> Bool) -> String -> String -> Map -> Integer
pl f _ a _ | f a = 0
pl f (d:sr) cur m = 1 + pl f sr next m
  where next = getNext d cur m
pl _ _ _ _ = undefined

pathLen :: String -> String -> Map -> Integer
pathLen = pl (== "ZZZ")

pathLen' :: String -> String -> Map -> Integer
pathLen' = pl ("Z" `isSuffixOf`)

getNext :: Char -> String -> Map -> String
getNext 'L' cur m = fst . fromJust $ lookup cur m
getNext 'R' cur m = snd . fromJust $ lookup cur m
getNext _ _ _ = undefined

part1 :: Network -> Integer
part1 (Network (path, m)) = pathLen path "AAA" m

part2 :: Network -> Integer
part2 (Network (path, m)) =
  let dists = map (\x -> pathLen' path x m) (filter ("A" `isSuffixOf`) (map fst m)) in
    foldl1 lcm dists

main :: IO ()
main = do
  path <- getDataFileName "day8.txt"
  result <- parseFromFile parseData path
  case result of
    Left err -> error $ show err
    Right file -> do
      print $ part1 file
      print $ part2 file
