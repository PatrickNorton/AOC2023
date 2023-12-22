module Main where

import Control.Monad
import Control.Monad.State
import Data.List
import Data.Map ((!?))
import qualified Data.Map as M
import Data.Maybe
import Text.Parsec hiding (State)
import Text.Parsec.String
import Util

import Paths_AOC2023_hs

type Point3d = (Int, Int, Int)
type Brick = (Point3d, Point3d)

line :: Parser Brick
line = do
  a1 <- number
  void $ char ','
  a2 <- number
  void $ char ','
  a3 <- number
  void $ char '~'
  b1 <- number
  void $ char ','
  b2 <- number
  void $ char ','
  b3 <- number
  void newline
  return ((a1, a2, a3), (b1, b2, b3))

minZ :: Brick -> Int
minZ ((_, _, z1), (_, _, z2)) = min z1 z2

droppedBrick :: M.Map (Int, Int) Int -> Brick -> Brick
droppedBrick tallest ((x1, y1, z1), (x2, y2, z2)) =
  let peak = maximum [fromMaybe 0 (tallest !? (x, y)) | x <- [x1..x2], y <- [y1..y2]]
      dz = max (z1 - peak - 1) 0 in
    ((x1, y1, z1 - dz), (x2, y2, z2 - dz))

wasDropped :: Brick -> Brick -> Bool
wasDropped ((_, _, za), _) ((_, _, zb), _) = za /= zb

drop1 :: Brick -> State (M.Map (Int, Int) Int) (Bool, Brick)
drop1 b = do
  tallest <- get
  let dropped@((dx1, dy1, dz1), (dx2, dy2, dz2)) = droppedBrick tallest b
  let fallen = wasDropped b dropped
  mapM_ (\(x, y) -> modify (M.insert (x, y) $ max dz1 dz2))
    $ cartProd [dx1..dx2] [dy1..dy2]
  return (fallen, dropped)

fall :: [Brick] -> (Int, [Brick])
fall bs = let bs' = sortOn minZ bs in
  let (res, _) = runState (mapM drop1 bs') M.empty in
    (length $ filter fst res, map snd res)

part1 :: [Brick] -> Int
part1 bs = let (_, dropped) = fall bs in
  length . filter ((==0) . fst) $ map (\i -> fall $ exceptIndex i dropped) (ind dropped)

part2 :: [Brick] -> Int
part2 bs = let (_, dropped) = fall bs in
  sum $ map (\i -> fst . fall $ exceptIndex i dropped) (ind dropped)

main :: IO ()
main = do
  path <- getDataFileName "day22.txt"
  vals <- parseFromFile (many1 line) path
  case vals of
    Left e -> error $ show e
    Right bricks -> do
      print $ part1 bricks
      print $ part2 bricks
