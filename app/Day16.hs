{-# LANGUAGE TupleSections #-}

module Main where

import Control.Monad.State
import qualified Data.Set as S
import Util

import Paths_AOC2023_hs

data Direction = North | South | West | East
  deriving (Show, Eq, Ord)

type Cache = S.Set ((Int, Int), Direction)
type Result = S.Set (Int, Int)

moveTo :: Direction -> (Int, Int) -> (Int, Int)
moveTo North (x, y) = (x - 1, y)
moveTo South (x, y) = (x + 1, y)
moveTo West (x, y) = (x, y - 1)
moveTo East (x, y) = (x, y + 1)

bounceDir :: Char -> Direction -> [Direction]
bounceDir '.' d = [d]
bounceDir '/' North = [East]
bounceDir '/' East = [North]
bounceDir '/' South = [West]
bounceDir '/' West = [South]
bounceDir '|' North = [North]
bounceDir '|' South = [South]
bounceDir '|' East = [North, South]
bounceDir '|' West = [North, South]
bounceDir '\\' North = [West]
bounceDir '\\' West = [North]
bounceDir '\\' South = [East]
bounceDir '\\' East = [South]
bounceDir '-' North = [East, West]
bounceDir '-' South = [East, West]
bounceDir '-' East = [East]
bounceDir '-' West = [West]
bounceDir c _ = error $ show c

bounce :: [String] -> (Int, Int) -> Direction -> State Cache Result
bounce table (x, y) dir = do
  s <- get
  if ((x, y), dir) `S.member` s
    then return S.empty
    else case index2d table (x, y) of
      Nothing -> return S.empty
      Just c -> let bs = bounceDir c dir in do
        modify (S.insert ((x, y), dir))
        res <- mapM (\b -> bounce table (moveTo b (x, y)) b) bs
        return . S.insert (x, y) $ S.unions res

litSquares :: [String] -> (Int, Int) -> Direction -> Result
litSquares table s dir = evalState (bounce table s dir) S.empty

part1 :: [String] -> Int
part1 table = S.size $ litSquares table (0, 0) East

part2 :: [String] -> Int
part2 table = let xs = [0..length table - 1]
                  ys = [0.. length (head table) - 1]
                  starts = map (,0,East) xs
                    ++ map (,length (head table) - 1,West) xs
                    ++ map (0,,South) ys
                    ++ map (length table - 1,,North) ys in
                maximum $ map (\(x,y,d) -> S.size $ litSquares table (x, y) d) starts

main :: IO ()
main = do
  file <- getDataFileName "day16.txt"
  dat <- lines <$> readFile file
  print $ part1 dat
  print $ part2 dat
