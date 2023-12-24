module Util ( number
            , hexnum
            , signed
            , enumerate
            , cartProd
            , cartSq
            , taxicab
            , countOf
            , ind
            , (<.>)
            , readLines
            , (!!?)
            , index2d
            , wrapIndex2d
            , both
            , bounds
            , replaceAssoc
            , exceptIndex
            ) where

import Data.Bifunctor
import Numeric
import Text.Parsec
import Text.Parsec.String (Parser)

number :: (Integral a) => (Read a) => Parser a
number = do
  digits <- many1 digit
  return $ read digits

hexnum :: (Eq a, Num a) => Parser a
hexnum = do
  digits <- many1 hexDigit
  return . fst . head $ readHex digits

signed :: (Integral a) => (Read a) => Parser a
signed = do
  char '-' *> (negate <$> number)
  <|> (char '+' *> number)
  <|> number

enumerate :: [a] -> [(Int, a)]
enumerate = zip [0..]

cartProd :: [a] -> [b] -> [(a, b)]
cartProd xs ys = [(x,y) | x <- xs, y <- ys]

cartSq :: [a] -> [(a, a)]
cartSq v = cartProd v v

taxicab :: (Int, Int) -> (Int, Int) -> Int
taxicab (x, y) (x', y') = abs (x - x') + abs (y - y')

countOf :: (a -> Bool) -> [a] -> Int
countOf f = length . filter f

ind :: [a] -> [Int]
ind = map fst . enumerate

infixr 9 <.>
(<.>) :: (Monad m) => (a -> b) -> (c -> m a) -> (c -> m b)
(f <.> g) a = f <$> g a

readLines :: String -> IO [String]
readLines = lines <.> readFile

infixr 9 !!?
(!!?) :: [a] -> Int -> Maybe a
[] !!? _ = Nothing
(a:_) !!? 0 = Just a
(_:rest) !!? i = rest !!? (i-1)

index2d :: [[a]] -> (Int, Int) -> Maybe a
index2d a (x, y) = (a !!? x) >>= (!!? y)

wrapIndex2d :: [[a]] -> (Int, Int) -> a
wrapIndex2d a (x, y) = (a !! (x `mod` n)) !! (y `mod` m)
  where n = length a
        m = length $ head a

both :: (Bifunctor t) => (a -> b) -> t a a -> t b b
both f = bimap f f

bounds :: [[a]] -> (Int, Int)
bounds g = (length g, length $ head g)

replaceAssoc :: (Eq a) => [(a, b)] -> a -> b -> [(a, b)]
replaceAssoc [] _ _ = []
replaceAssoc ((a, b):rest) a' b' | a == a' = (a', b'):rest
                                 | otherwise = (a, b):replaceAssoc rest a' b'

exceptIndex :: Int -> [a] -> [a]
exceptIndex _ [] = []
exceptIndex 0 (_:rest) = rest
exceptIndex i (a:rest) = a : exceptIndex (i - 1) rest
