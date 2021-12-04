-- https://adventofcode.com/2021/day/4

import Data.List
import Data.Maybe


-- Board with lines and columns of (un)marked values
data Value = Unmarked Int | Marked Int

data Board = Board [[Value]] [[Value]]

mkBoard :: [[Int]] -> Board
mkBoard xs = Board ls cs
    where
        ls = map (map Unmarked) xs
        cs = transpose ls

wins :: Board -> Bool
wins (Board ls cs) = any winning ls || any winning cs
    where
        winning           = all marked
        marked (Marked _) = True
        marked _          = False

mark :: Int -> Board -> Board
mark x (Board ls cs) = Board (go ls) (go cs)
    where
        go = map (map m)
        m (Unmarked v) | x == v = Marked v
        m v                     = v

score :: Int -> Board -> Int
score x (Board ls _) = x * sum (concatMap getUnmarked ls)
    where
        getUnmarked = mapMaybe get
        get (Unmarked v) = Just v
        get _            = Nothing


parseInts :: String -> [Int]
parseInts = map read . words

parseRandom :: String -> [Int]
parseRandom = parseInts . map (\c -> if c == ',' then ' ' else c)

parseBoards :: [String] -> [Board]
parseBoards = map mkBoard . foldl parse []
    where
        parse acc    ""   = []:acc
        parse (b:bs) line = (parseInts line:b):bs
        parse _      _    = undefined


-- Play a board returning a tuple of step and score in case it wins
play :: Num t => t -> [Int] -> Board -> Maybe (t, Int)
play _   []     _ = Nothing
play cnt (r:rs) b =
    if wins b'
    then Just (cnt, score r b')
    else play (cnt+1) rs b'
    where
        b' = mark r b


main :: IO ()
main = do
    (rnd : boards) <- lines <$> readFile  "day_4_input.txt"
    let rs = parseRandom rnd
    let bs = parseBoards boards
    let winningStates = mapMaybe (play 1 rs) bs

    let firstWinningScore = snd $ head $ sortOn fst winningStates
    putStrLn $ "#1: " ++ show firstWinningScore

    let lastWinningScore = snd $ last $ sortOn fst winningStates
    putStrLn $ "#2: " ++ show lastWinningScore
