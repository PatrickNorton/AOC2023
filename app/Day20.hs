module Main where

import Data.Map (Map, fromList, (!), adjust, elems, empty, keys, insert, member, notMember)
import Data.List (find)
import Data.Maybe (fromJust)

import Paths_AOC2023_hs

data State  = On   | Off deriving (Show, Eq)
data Pulse  = High | Low deriving (Show, Eq)
data Module = Broadcaster { connected :: [String] }         |
              Flip { state :: State, connected :: [String]} |
              Conjunction { inputs :: Map String Pulse, connected :: [String] } deriving (Show, Eq)

data Iteration = Iteration { modules :: Map String Module
                           , presses :: Int
                           , cycling :: Map String Int
                           , lows :: Int
                           , highs :: Int }
                    deriving (Show, Eq)

isConjunction :: Module -> Bool
isConjunction (Conjunction _ _) = True
isConjunction _                 = False

parseInput :: String -> Map String Module
parseInput input  = foldr getInputs mods $ keys mods
    where mods = fromList . map (parseLine . words) . lines $ input
          getInputs name ms
            = foldr (adjust (addInput name)) ms
                [child | child <- connected (ms ! name)
                  , child `member` ms
                  , isConjunction  (ms ! child)]
          addInput i m@(Conjunction {inputs = inp}) = m { inputs = insert i Low inp }
          addInput _ _ = undefined
          parseLine (name : _ : xs)
            | head name == '&' = (tail name, Conjunction { inputs=empty, connected=children })
            | head name == '%' = (tail name, Flip { state=Off, connected=children })
            | otherwise = (name, Broadcaster { connected=children })
            where children = map (filter (/= ',')) xs
          parseLine _ = undefined

doIteration :: [(String, String, Pulse)] -> Iteration -> Iteration
doIteration [] it = it
doIteration [(_, "broadcaster", Low)] it
  = doIteration [("broadcaster", c, Low) | c <- children] (it { lows=l' })
  where children = connected $ modules it ! "broadcaster"
        l' = lows it + length children
doIteration ((sender, name, pulse):queue) it
  | name `notMember` modules it = doIteration queue it
  | pulse == High && not (isConjunction m) = doIteration queue it { cycling = c' }
  | otherwise = doIteration queue' it'
  where m = modules it ! name
        -- Update cycle length of inputs of conjunction module connected to rx
        c' | pulse /= High || "rx" `notElem` connected m = cycling it
           | otherwise = insert sender (presses it) (cycling it)

        -- Update the module
        m' | isConjunction m = m { inputs = adjust (const pulse) sender (inputs m) }
           | state m == On = m { state = Off }
           | otherwise = m { state = On }

        -- Get the next pulse to send
        getPulse (Conjunction ins _) | all (== High) $ elems ins = Low
                                     | otherwise                 = High
        getPulse (Flip On _) = High
        getPulse (Flip Off _) = Low
        getPulse _ = undefined
        p' = getPulse m'

        -- Add this pulse to the send queue
        queue' = queue ++ [(name, child, p') | child <- connected m']

        -- Update the iteration: modules and counters
        l'     | p' == High = lows  it
               | otherwise  = lows  it + length (connected m')
        h'     | p' == Low  = highs it
               | otherwise  = highs it + length (connected m')
        mods'  = adjust (const m') name $ modules it
        it'    = it { modules = mods', cycling = c', lows = l', highs = h' }

doNextIteration :: Iteration -> Iteration
doNextIteration (Iteration it n c _ _)
  = doIteration [("button", "broadcaster", Low)] (Iteration it (n + 1) c 1 0)

part1 :: Map String Module -> Int
part1 input = (sum . map lows) iterations * (sum . map highs) iterations
    where iterations = take 1000 . tail . iterate doNextIteration $ Iteration input 0 empty 0 0

part2 :: Map String Module -> Int
part2 input = foldr1 lcm . elems $ cycling iterations
    where mustBeHigh = keys . inputs . fromJust . find (("rx" `elem`) . connected) . elems $ input
          allFound it = all (`elem` (keys $ cycling it)) mustBeHigh
          iterations = head . dropWhile (not . allFound)
            . tail . iterate doNextIteration $ Iteration input 0 empty 0 0

main :: IO ()
main = do
  path <- getDataFileName "day20.txt"
  input <- parseInput <$> readFile path
  print $ part1 input
  print $ part2 input
