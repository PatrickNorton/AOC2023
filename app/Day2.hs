module Main where

import Control.Monad
import Text.Parsec
import Text.Parsec.String (Parser, parseFromFile)

import Paths_AOC2023_hs

data Color = Red | Green | Blue

data Game = Game
  { tag :: Int
  , cubes :: [[(Int, Color)]]
  }

number :: (Integral a) => (Read a) => Parser a
number = do
  digits <- many1 digit
  return $ read digits

parseData :: Parser [Game]
parseData = many1 parseLine

parseLine :: Parser Game
parseLine = do
  t <- parseTag
  c <- parseCubes
  void newline
  return $ Game {tag = t, cubes = c}

parseTag :: Parser Int
parseTag = do
  void $ string "Game "
  t <- number
  void $ string ": "
  return t

parseCubes :: Parser [[(Int, Color)]]
parseCubes = parseCube `sepBy` string "; "

parseCube :: Parser [(Int, Color)]
parseCube = parseVal `sepBy` string ", "

parseVal :: Parser (Int, Color)
parseVal = do
  n <- number
  void $ string " "
  c <- parseColor
  return (n, c)

parseColor :: Parser Color
parseColor = parseC "red" Red <|> parseC "green" Green <|> parseC "blue" Blue

parseC :: String -> Color -> Parser Color
parseC s c = do
  void $ string s
  return c

sum3 :: [(Int, Int, Int)] -> (Int, Int, Int)
sum3 [] = (0, 0, 0)
sum3 ((a, b, c):rest) = let (as, bs, cs) = sum3 rest in (a + as, b + bs, c + cs)

convertColors :: [(Int, Color)] -> (Int, Int, Int)
convertColors [] = (0, 0, 0)
convertColors ((r, Red):rest) =
  let (rs, gs, bs) = convertColors rest in
    (max rs r, gs, bs)
convertColors ((g, Green):rest) =
  let (rs, gs, bs) = convertColors rest in
    (rs, max gs g, bs)
convertColors ((b, Blue):rest) =
  let (rs, gs, bs) = convertColors rest in
    (rs, gs, max bs b)

gameIsPossible :: Game -> Bool
gameIsPossible (Game {tag = _, cubes = c}) =
  let (rs, gs, bs) = convertColors $ concat c in
    rs <= 12 && bs <= 14 && gs <= 13

prod3 :: (Int, Int, Int) -> Int
prod3 (a, b, c) = a * b * c

part1 :: [Game] -> Int
part1 = sum . map tag . filter gameIsPossible

part2 :: [Game] -> Int
part2 = sum . map (prod3 . convertColors . concat . cubes)

main :: IO ()
main = do
  path <- getDataFileName "day2.txt"
  result <- parseFromFile parseData path
  case result of
    Left err -> error $ show err
    Right file -> do
      print $ part1 file
      print $ part2 file
      return ()
