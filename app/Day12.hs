module Main where

import Control.Monad
import Data.Bifunctor
import Data.Functor
import Data.List
import Data.Map ((!))
import qualified Data.Map as M
import Text.Parsec
import Text.Parsec.String
import Util

import Paths_AOC2023_hs

type Condition = ([Maybe Bool], [Int])

state :: Parser (Maybe Bool)
state = (char '.' $> Just True) <|> (char '#' $> Just False) <|> (char '?' $> Nothing)

states :: Parser [Maybe Bool]
states = many1 state

countHead :: (a -> Bool) -> [a] -> Int
countHead _ [] = 0
countHead f (a:rest) = if f a then 1 + countHead f rest else 0

line :: Parser Condition
line = do
  s <- states
  void $ char ' '
  v <- number `sepBy` char ','
  void newline
  return (s, v)

combos :: Condition -> Int
combos (input, pat) = cache ! (input, 0, pat)
 where
  cache = M.fromList
    [ ((r, n, p), help (r, n, p))
    | r <- tails input
    , n <- [0 .. maximum pat]
    , p <- tails pat
    ]
  help :: ([Maybe Bool], Int, [Int]) -> Int
  help (i, 0, []) = if Just False `notElem` i then 1 else 0
  help (_, _, []) = 0
  help ([], n, [p]) = if n == p then 1 else 0
  help ([], _, _) = 0
  help (Just True : r, 0, p : ps) = cache ! (r, 0, p : ps)
  help (Just True : r, n, p : ps) | n == p = cache ! (r, 0, ps)
                                  | otherwise = 0
  help (Just False : r, n, p : ps) | n == p = 0
                                   | otherwise = cache ! (r, n + 1, p : ps)
  help (Nothing : r, n, p : ps)
    | n == 0 = cache ! (r, 1, p : ps) + cache ! (r, 0, p : ps)
    | n < p  = cache ! (r, n + 1, p : ps)
    | n == p = cache ! (r, 0, ps)
  help p = error $ show p

expandStates :: [Maybe Bool] -> [Maybe Bool]
expandStates = concat . replicate 5

expandVals :: [Int] -> [Int]
expandVals [] = []
expandVals (a:rest) = a:a:a:a:a:expandVals rest

expandRow :: Condition -> Condition
expandRow = bimap expandStates expandVals

part1 :: [Condition] -> Int
part1 = sum . map combos

part2 :: [Condition] -> Int
part2 = sum . map (combos . expandRow)

main :: IO ()
main = do
  path <- getDataFileName "day12.txt"
  file <- parseFromFile (many1 line) path
  case file of
    Left e -> error $ show e
    Right d -> do
      print $ part1 d
      print $ part2 d
