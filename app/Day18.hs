module Main where

import Control.Monad
import Data.Bifunctor
import Data.Functor
import Numeric
import Text.Parsec
import Text.Parsec.String
import Util

import Paths_AOC2023_hs

data Direction = U | D | L | R

type DigInst = (Direction, Int, String)

moveN :: Direction -> (Int, Int) -> Int -> (Int, Int)
moveN U (x, y) n = (x - n, y)
moveN D (x, y) n = (x + n, y)
moveN L (x, y) n = (x, y - n)
moveN R (x, y) n = (x, y + n)

direct :: Parser Direction
direct = (char 'U' $> U) <|> (char 'D' $> D) <|> (char 'L' $> L) <|> (char 'R' $> R)

line :: Parser DigInst
line = do
  d <- direct
  void $ char ' '
  c <- number
  void $ char ' '
  void $ string "(#"
  col <- many1 hexDigit
  void $ string ")"
  void newline
  return (d, c, col)

digPlan :: Parser [DigInst]
digPlan = many1 line

vertices :: [DigInst] -> [(Int, Int)]
vertices = inner (0, 0)
  where inner :: (Int, Int) -> [DigInst] -> [(Int, Int)]
        inner pt [] = [pt]
        inner pt ((d, c, _):rest) =
          let movement = moveN d pt c in
            pt : inner movement rest

perimLen :: [DigInst] -> Int
perimLen v = 1 + sum (map (\(_, a, _) -> a) v)

shoelace :: [(Int, Int)] -> Int
shoelace [] = undefined
shoelace (h:rest) = abs (inner (h:rest ++ [h])) `div` 2
  where inner [] = undefined
        inner [_] = 0
        inner ((x1, y1):(x2, y2):r) = (x1*y2) - (x2*y1) + inner ((x2, y2):r)

interSize :: [(Int, Int)] -> Int -> Int
interSize pts perim = shoelace pts + perim `div` 2 + 1

splitLast :: [a] -> ([a], a)
splitLast [] = undefined
splitLast [a] = ([], a)
splitLast (a:rest) = first (a:) $ splitLast rest

digToDir :: Char -> Direction
digToDir '0' = R
digToDir '1' = D
digToDir '2' = L
digToDir '3' = U
digToDir _ = undefined

changeInstr :: DigInst -> DigInst
changeInstr (_, _, h) = let (n, d) = splitLast h
                            num = fst . head $ readHex n in
                          (digToDir d, num, h)

part1 :: [DigInst] -> Int
part1 vals = interSize (vertices vals) (perimLen vals)

part2 :: [DigInst] -> Int
part2 = part1 . map changeInstr

main :: IO ()
main = do
  path <- getDataFileName "day18.txt"
  parsed <- parseFromFile digPlan path
  case parsed of
     (Left e) -> error $ show e
     (Right plan) -> do
       print $ part1 plan
       print $ part2 plan
