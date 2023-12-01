module Main where

digsToInt :: String -> Int
digsToInt s = read [head s, last s]

part1 :: [String] -> Int
part1 = sum . map (digsToInt . filter (`elem` ['0'..'9']))

splitString :: String -> String
splitString [] = []
splitString ('1':rest) = '1':splitString rest
splitString ('2':rest) = '2':splitString rest
splitString ('3':rest) = '3':splitString rest
splitString ('4':rest) = '4':splitString rest
splitString ('5':rest) = '5':splitString rest
splitString ('6':rest) = '6':splitString rest
splitString ('7':rest) = '7':splitString rest
splitString ('8':rest) = '8':splitString rest
splitString ('9':rest) = '9':splitString rest
splitString ('o':'n':'e':rest) = '1':splitString ('e':rest)
splitString ('t':'w':'o':rest) = '2':splitString ('o':rest)
splitString ('t':'h':'r':'e':'e':rest) = '3':splitString rest
splitString ('f':'o':'u':'r':rest) = '4':splitString rest
splitString ('f':'i':'v':'e':rest) = '5':splitString ('e':rest)
splitString ('s':'i':'x':rest) = '6':splitString rest
splitString ('s':'e':'v':'e':'n':rest) = '7':splitString ('n':rest)
splitString ('e':'i':'g':'h':'t':rest) = '8':splitString ('t':rest)
splitString ('n':'i':'n':'e':rest) = '9':splitString ('e':rest)
splitString (_:rest) = splitString rest

part2 :: [String] -> Int
part2 = part1 . map splitString

main :: IO ()
main = do
  file <- readFile "data/day1.txt"
  let l = lines file
  print $ part1 l
  print $ part2 l
