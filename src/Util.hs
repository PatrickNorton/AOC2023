module Util ( number
            , enumerate
            , cartProd
            , taxicab
            , countOf
            , ind
            , (<.>)
            , readLines
            ) where

import Text.Parsec
import Text.Parsec.String (Parser)

number :: (Integral a) => (Read a) => Parser a
number = do
  digits <- many1 digit
  return $ read digits

enumerate :: [a] -> [(Int, a)]
enumerate = zip [0..]

cartProd :: [a] -> [b] -> [(a, b)]
cartProd xs ys = [(x,y) | x <- xs, y <- ys]

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
