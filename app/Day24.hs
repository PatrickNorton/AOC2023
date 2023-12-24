module Main (main) where

import Control.Monad
import Data.Matrix (Matrix)
import qualified Data.Matrix as M
import Data.Maybe
import qualified Data.Vector as V
import Text.Parsec
import Text.Parsec.String
import Util

import Paths_AOC2023_hs

type Point3d = (Integer, Integer, Integer)
type Hailstone = (Point3d, Point3d)

divR :: Integer -> Integer -> Rational
divR x y = toRational x / toRational y

tripleToList :: (a, a, a) -> [a]
tripleToList (a, b, c) = [a, b, c]

cross :: (Integral a) => (a, a, a) -> (a, a, a) -> (a, a, a)
cross (a1, a2, a3) (b1, b2, b3) =
  (a2 * b3 - a3 * b2, a3 * b1 - a1 * b3, a1 * b2 - a2 * b1)

crossMatrix :: (Num a) => (a, a, a) -> Matrix a
crossMatrix (a, b, c) =
  M.fromLists [[0, -c, b], [c, 0, -a], [-b, a, 0]]

tryDivR :: Integer -> Integer -> Maybe Rational
tryDivR x y = do
  guard (y /= 0)
  return (x `divR` y)

hailstone :: Parser Hailstone
hailstone = do
  px <- number
  void $ string ", "
  py <- number
  void $ string ", "
  pz <- number
  void $ string " @ "
  dx <- signed
  void $ string ", "
  dy <- signed
  void $ string ", "
  dz <- signed
  void newline
  return ((px, py, pz), (dx, dy, dz))

intersectForward :: Hailstone -> Hailstone -> Maybe (Rational, Rational)
intersectForward h1@((px1, py1, _), (dx1, dy1, _))
                 h2@((px2, py2, _), (dx2, dy2, _)) = do
  guard (h1 /= h2)
  tx <- ((py1 - py2) * dx1 - (px1 - px2) * dy1) `tryDivR` (dx1 * dy2 - dy1 * dx2)
  ty <- ((py1 - py2) * dx2 - (px1 - px2) * dy2) `tryDivR` (dx1 * dy2 - dy1 * dx2)
  guard (tx >= 0 && ty >= 0)
  let xi = toRational px1 + toRational dx1 * ty
  let yi = toRational py1 + toRational dy1 * ty
  return (xi, yi)

inBounds :: (Rational, Rational) -> Bool
inBounds (x, y) = x >= 200000000000000 && x <= 400000000000000
                  && y >= 200000000000000 && y <= 400000000000000

-- Linear algebra time!
-- If we have a rock with position p0 and velocity v0 and hailstones p[i] and
-- v[i], then p0 + t[i]v0 == p[1] + t[i]v[i]; equivalently
-- (p0 - p[i]) x (v0 - v[i]) == 0.
-- Since p0 x v0 is common to every equation, we only need the first 3
-- hailstones (i=1, 2 and i=1, 3) to get a 6x6 system of linear equations!
intersectMatrix :: [Hailstone] -> Matrix Rational
intersectMatrix (h0:h1:h2:_) =
  let p0 = M.colVector . V.fromList $ tripleToList (uncurry cross h0)
      p1 = M.colVector . V.fromList $ tripleToList (uncurry cross h1)
      p2 = M.colVector . V.fromList $ tripleToList (uncurry cross h2)
      rhs = (-p0 + p1) M.<-> (-p0 + p2)
      m00 = crossMatrix (snd h0) - crossMatrix (snd h1)
      m30 = crossMatrix (snd h0) - crossMatrix (snd h2)
      m03 = -crossMatrix (fst h0) + crossMatrix (fst h1)
      m33 = -crossMatrix (fst h0) + crossMatrix (fst h2)
      m' = (m00 M.<-> m30) M.<|> (m03 M.<-> m33)
      m = toRational <$> m' in
  case M.inverse m of
    Left e -> error $ show e
    Right mx -> mx * (toRational <$> rhs)
intersectMatrix _ = error "Not enough hailstones"

part1 :: [Hailstone] -> Int
part1 = (`div` 2) . length . filter inBounds . mapMaybe (uncurry intersectForward) . cartSq

part2 :: [Hailstone] -> Int
part2 = sum . take 3 . fmap ceiling . M.toList . intersectMatrix

main :: IO ()
main = do
  path <- getDataFileName "day24.txt"
  vs <- parseFromFile (many1 hailstone) path
  case vs of
    Left e -> error $ show e
    Right stones -> do
      print $ part1 stones
      print $ part2 stones
