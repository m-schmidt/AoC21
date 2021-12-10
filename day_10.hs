-- https://adventofcode.com/2021/day10

import Data.List ( foldl', sort )
import Data.Maybe ( mapMaybe )


data Score
    = Ok
    | Corrupted Int
    | Incomplete Int

getCorrupt :: Score -> Maybe Int
getCorrupt (Corrupted n) = Just n
getCorrupt _ = Nothing

getIncomplete :: Score -> Maybe Int
getIncomplete (Incomplete n) = Just n
getIncomplete _ = Nothing


isOpening :: Char -> Bool
isOpening c = case c of
    '(' -> True
    '[' -> True
    '{' -> True
    '<' -> True
    _   -> False

opposite :: Char -> Char
opposite c = case c of
    '(' -> ')'
    '[' -> ']'
    '{' -> '}'
    '<' -> '>'
    _   -> undefined

score1 :: Char -> Int
score1 c = case c of
    ')' -> 3
    ']' -> 57
    '}' -> 1197
    '>' -> 25137
    _   -> undefined

score2 :: Char -> Int
score2 c = case c of
    ')' -> 1
    ']' -> 2
    '}' -> 3
    '>' -> 4
    _   -> undefined

scoreLine :: String -> Score
scoreLine = go []
   where
       go acc (c:cs) | isOpening c = go (opposite c : acc) cs -- open chunk
       go (c':acc) (c:cs) | c' == c = go acc cs   -- close chunk

       go []  []    = Ok
       go _   (c:_) = Corrupted $ score1 c
       go acc []    = Incomplete $ foldl' (\a c -> a*5 + score2 c) 0 acc


solve1 :: [String] -> Int
solve1 = sum . mapMaybe (getCorrupt . scoreLine)

solve2 :: [String] -> Int
solve2 = median . mapMaybe (getIncomplete . scoreLine)
    where median l = sort l !! (length l `div` 2)

main :: IO ()
main = do
    lns <- lines <$> readFile  "day_10_input.txt"
    putStrLn $ "#1: " ++ show (solve1 lns)
    putStrLn $ "#2: " ++ show (solve2 lns)
