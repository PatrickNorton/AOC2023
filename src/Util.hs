module Util (number, enumerate, cartProd) where

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
