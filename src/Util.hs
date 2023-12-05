module Util where

import Text.Parsec
import Text.Parsec.String (Parser)

number :: (Integral a) => (Read a) => Parser a
number = do
  digits <- many1 digit
  return $ read digits
