module Main where

import Data.Char
import Data.Functor
import Data.Maybe
import Text.Parsec
import Text.Parsec.String
import Util

import Paths_AOC2023_hs

type Value = (String, Maybe Int)

values :: Parser [String]
values = many1 (noneOf ",\n") `sepBy` string ","

val :: Parser Value
val = do
  s <- many1 lower
  c <- (char '-' $> Nothing) <|> ((char '=' *> number) <&> Just)
  return (s, c)

values' :: Parser [Value]
values' = val `sepBy` string ","

doToIndex :: (a -> a) -> Int -> [a] -> [a]
doToIndex _ _ [] = undefined
doToIndex f 0 (a:rest) = f a:rest
doToIndex f i (a:rest) = a:doToIndex f (i-1) rest

hash :: String -> Int
hash = foldl (\cur x -> ((cur + ord x) * 17) `mod` 256) 0

remLabel :: String -> [Value] -> [Value]
remLabel s = filter (\(x, _) -> x /= s)

swapLabel :: Value -> [Value] -> [Value]
swapLabel v [] = [v]
swapLabel v (a:rest) | fst v == fst a = v:rest
                     | otherwise = a:swapLabel v rest

doDash :: [[Value]] -> String -> [[Value]]
doDash vs lbl = doToIndex (remLabel lbl) (hash lbl) vs

doEquals :: [[Value]] -> Value -> [[Value]]
doEquals vs v = doToIndex (swapLabel v) (hash $ fst v) vs

doLabel :: [[Value]] -> Value -> [[Value]]
doLabel vs v@(lbl, f) = case f of
  Nothing -> doDash vs lbl
  Just _ -> doEquals vs v

focusPower :: Int -> [Value] -> Int
focusPower b = sum . zipWith (\i (_,l) -> b*i*fromJust l) [1..]

part1 :: [String] -> Int
part1 = sum . map hash

part2 :: [Value] -> Int
part2 vs = let hm = replicate 256 []
               final = foldl doLabel hm vs in
             sum $ zipWith focusPower [1..] final

main :: IO ()
main = do
  path <- getDataFileName "day15.txt"
  result <- parseFromFile values path
  result' <- parseFromFile values' path
  case (result, result') of
    (Right file, Right file') -> do
      print $ part1 file
      print $ part2 file'
    _ -> undefined
