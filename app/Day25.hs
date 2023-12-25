module Main where

import Control.Monad
import Control.Monad.State
import Data.Map ((!))
import qualified Data.Map as M
import System.Random
import Text.Parsec hiding (choice, State)
import Text.Parsec.String

import Paths_AOC2023_hs

newtype KargerState = KargerState StdGen

pair :: Parser (String, [String])
pair = do
    node <- many1 letter
    void $ char ':'
    void spaces
    children <- many1 letter `sepBy` char ' '
    void spaces
    return (node, children)

input :: Parser (M.Map String [String])
input = do
    pairs <- many1 pair
    let g = M.fromList pairs
    return $ foldl (\g' (n, ns) -> foldl (\g'' n' -> M.insertWith (++) n' [n] g'') g' ns) g pairs

choice :: StdGen -> [a] -> (a, StdGen)
choice _ [] = error "choice: empty list"
choice g xs = (xs !! i, g')
    where (i, g') = randomR (0, length xs - 1) g

contract :: String -> String -> (M.Map String [String], M.Map String [String])
  -> (M.Map String [String], M.Map String [String])
contract u v (g, s) =
    let g' = M.delete v $ M.delete u g
        g'' = M.insert u (g ! u ++ g ! v) g'
        g''' = M.map (map (\x -> if x == v then u else x)) g''
        g'''' = M.update (Just . filter (/= u)) u g'''
        s' = M.delete v $ M.delete u s
        s'' = M.insert u (s ! u ++ s ! v) s'
    in (g'''', s'')

minCutM :: M.Map String [String] -> M.Map String [String]
  -> State KargerState (Int, M.Map String [String])
minCutM graph s = do
    KargerState g <- get
    if M.size graph <= 2
        then return (length $ head $ M.elems graph, s)
        else do
            let keys = M.keys graph
                (u, g') = choice g keys
                (v, g'') = choice g' $ filter (`M.member` graph)  $ graph M.! u
                (graph', s') = contract u v (graph, s)
            put $ KargerState g''
            minCutM graph' s'

karger :: M.Map String [String] -> (Int, M.Map String [String])
karger graph = head $ dropWhile (\(c, _) -> c > 3)
  $ evalState (replicateM (length graph * length graph) (minCutM graph s))
  (KargerState $ mkStdGen 0)
    where s = M.fromList $ zip (M.keys graph) (map (:[]) $ M.keys graph)

part1 :: M.Map String [String] -> Int
part1 = product . M.map length . snd . karger

main :: IO ()
main = do
  path <- getDataFileName "day25.txt"
  dat <- parseFromFile input path
  case dat of
    Left e -> error $ show e
    Right e -> print $ part1 e
