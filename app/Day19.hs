{-# LANGUAGE TupleSections #-}

module Main where

import Control.Monad
import Data.Bifunctor
import Data.Functor
import Data.Maybe
import Text.Parsec
import Text.Parsec.String
import Util

import Paths_AOC2023_hs

data Cmp = LessThan | GreaterThan

data Action = Accept
            | Reject
            | RunWorkflow String

type Rule' = (String, Cmp, Int, Action)

data Rule = Proper Rule' | Direct Action

type Workflow = (String, [Rule])

data PartRange = PartRange (Int, Int) (Int, Int) (Int, Int) (Int, Int)
  deriving (Show, Eq)

cmpFn :: Cmp -> Int -> Int -> Bool
cmpFn LessThan = (<)
cmpFn GreaterThan = (>)

cmp :: Parser Cmp
cmp = (char '<' $> LessThan) <|> (char '>' $> GreaterThan)

action :: Parser Action
action = (char 'A' $> Accept) <|> (char 'R' $> Reject) <|> (RunWorkflow <$> many1 lower)

properRule :: Parser Rule'
properRule = do
  var <- many1 lower
  c <- cmp
  num <- number
  void $ char ':'
  act <- action
  return (var, c, num, act)

rule :: Parser Rule
rule = (Proper <$> try properRule) <|> (Direct <$> action)

workflow :: Parser Workflow
workflow = do
  name <- many1 lower
  void $ char '{'
  rules <- rule `sepBy` char ','
  void $ char '}'
  void newline
  return (name, rules)

rating :: Parser (String, Int)
rating = do
  s <- many1 lower
  void $ char '='
  n <- number
  return (s, n)

ratings :: Parser [(String, Int)]
ratings = do
  void $ char '{'
  r <- rating `sepBy` char ','
  void $ char '}'
  void newline
  return r

parser :: Parser ([Workflow], [[(String, Int)]])
parser = do
  ws <- many1 workflow
  void newline
  rs <- many1 ratings
  return (ws, rs)

workflowAction :: [(String, Int)] -> [Rule] -> Action
workflowAction _ [] = undefined
workflowAction _ (Direct a:_) = a
workflowAction vs (Proper (s, c, i, a):rest) =
  let p = fromJust $ lookup s vs in
    if cmpFn c p i then a else workflowAction vs rest

isAccepted :: [Workflow] -> String -> [(String, Int)] -> Bool
isAccepted w s r =
  let rules = fromJust $ lookup s w in
    case workflowAction r rules of
      Accept -> True
      Reject -> False
      RunWorkflow s' -> isAccepted w s' r

partRating :: [(String, Int)] -> Int
partRating = sum . map snd

rangeSize :: (Int, Int) -> Int
rangeSize (a, b) = b - a + 1

permSum :: [(Int, Int)] -> Int
permSum = product . map rangeSize

splitRange :: (Int, Int) -> Int -> ((Int, Int), Maybe (Int, Int))
splitRange (a, b) x | x >= b = ((a, b), Nothing)
                    | x <= a = ((a, b), Nothing)
                    | otherwise = ((a, x), Just (x+1, b))

splitByCmp :: (Int, Int) -> Cmp -> Int -> (Maybe (Int, Int), Maybe (Int, Int))
splitByCmp (a, b) LessThan x = first Just $ splitRange (a, b) (x-1)
splitByCmp (a, b) GreaterThan x =
  let (v1, v2) = splitRange (a, b) x in
    (v2, Just v1)

acceptInner :: [Workflow] -> [Rule] -> [(String, (Int, Int))] -> Int
acceptInner _ [] _ = undefined
acceptInner _ (Direct Accept:_) r  = permSum $ map snd r
acceptInner _ (Direct Reject:_) _ = 0
acceptInner ws (Direct (RunWorkflow w):_) r = countAccepted ws w r
acceptInner ws (Proper (s, c, i, a):rest) r =
  let v = fromJust $ lookup s r
      (y, n) = splitByCmp v c i
      inner r' v' = acceptInner ws r' (replaceAssoc r s v')
      inner' r' = maybe 0 (inner r') in
    inner' (Direct a:rest) y + inner' rest n

countAccepted :: [Workflow] -> String -> [(String, (Int, Int))] -> Int
countAccepted w s r =
  let rules = fromJust $ lookup s w in
    acceptInner w rules r

part1 :: ([Workflow], [[(String, Int)]]) -> Int
part1 (ws, pts) = sum . map partRating . filter (isAccepted ws "in") $ pts

part2 :: ([Workflow], [[(String, Int)]]) -> Int
part2 (ws, _) = countAccepted ws "in" $ map (,(1,4000)) ["x", "m", "a", "s"]

main :: IO ()
main = do
  path <- getDataFileName "day19.txt"
  d <- parseFromFile parser path
  case d of
    Left e -> error $ show e
    Right v -> do
      print $ part1 v
      print $ part2 v
