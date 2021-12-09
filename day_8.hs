-- https://adventofcode.com/2021/day/8

import Data.List ( sort, find, nub )
import qualified Data.Set as S
import qualified Data.Map.Strict as M
import Data.Maybe ( fromJust )


parse :: String -> ([S.Set Char], [S.Set Char])
parse s = (take 10 ws, drop 11 ws)
    where ws = map S.fromList $ words s

solve1 :: [[S.Set Char]] -> Int
solve1 xs = length $ filter isUnique $ concat xs
    where
        isUnique x = case S.size x of
            2 -> True
            4 -> True
            3 -> True
            7 -> True
            _ -> False

-- Compute translation map
decrypt :: [S.Set Char] -> M.Map (S.Set Char) Int
decrypt ws =
    -- words for the the unique digits
    let d1 = fromJust $ find (len 2) ws in
    let d4 = fromJust $ find (len 4) ws in
    let d7 = fromJust $ find (len 3) ws in
    let d8 = fromJust $ find (len 7) ws in
    -- words of lengths 5 and 6
    let ws5 = filter (len 5) ws in
    let ws6 = filter (len 6) ws in
    -- d3 and d9 are unique supersets of d1 within their respective groups ws5/ws6
    let d3 = fromJust $ find (`contains` d1) ws5 in
    let d9 = fromJust $ find (`contains` d3) ws6 in
    -- d6 is the only digit in ws6 that is not a superset of d1
    let d6 = fromJust $ find (\d -> not (d `contains` d1)) ws6 in
    -- d0 is the remaining element in ws6
    let d0 = fromJust $ find (\d -> d /= d6 && d /= d9) ws6 in
    -- d5 merged with d1 is equal to c9
    let d5 = fromJust $ find (\d -> d9 == S.union d d1) ws5 in
    -- d2 is the remaining element in ws5
    let d2 = fromJust $ find (\d -> d /= d3 && d /= d5) ws5 in

    M.fromList [ (d0,0), (d1,1), (d2,2), (d3,3), (d4,4), (d5,5), (d6,6), (d7,7), (d8,8), (d9,9) ]
    where
        len n w        = S.size w == n
        w `contains` v = v `S.isSubsetOf` w


decode :: ([S.Set Char], [S.Set Char]) -> Int
decode (signals, output) = toInt $ map toDigit output
    where
        tm        = decrypt signals
        toDigit x = tm M.! x
        toInt     = foldl (\a d -> a*10 + d) 0

solve2 :: [([S.Set Char], [S.Set Char])] -> Int
solve2 lns = sum $ map decode lns


main :: IO ()
main = do
    outputs <- map parse . lines <$> readFile  "day_8_input.txt"
    putStrLn $ "#1: " ++ show (solve1 $ map snd outputs)
    putStrLn $ "#2: " ++ show (solve2 outputs)
