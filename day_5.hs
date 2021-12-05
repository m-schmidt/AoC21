-- https://adventofcode.com/2021/day/5

import Data.List
import qualified Data.Set as S


data Line
    = H Int Int Int
    | V Int Int Int
    | X Int Int Int Int
    deriving (Show, Eq)

mkLine :: [Int] -> Line
mkLine [x1,y1,x2,y2]
    | y1 == y2 = H y1 (min x1 x2) (max x1 x2)
    | x1 == x2 = V x1 (min y1 y2) (max y1 y2)
    | x1 < x2  = X x1 y1 x2 y2
    | x2 < x1  = X x2 y2 x1 y1
mkLine _ = undefined

isHV :: Line -> Bool
isHV (H _ _ _) = True
isHV (V _ _ _) = True
isHV _ = False


points :: Line -> S.Set (Int, Int)
points (H y x1 x2)     = S.fromList [ (x,y) | x <- [x1..x2] ]
points (V x y1 y2)     = S.fromList [ (x,y) | y <- [y1..y2] ]
points (X x1 y1 x2 y2) = S.fromList [ (x,y) | x <- [x1..x2], let y = y1 + signum (y2-y1) * (x-x1)]

commonPoints :: Line -> Line -> S.Set (Int, Int)
commonPoints l1 l2 = S.intersection (points l1) (points l2)

countCommons :: [Line] -> Int
countCommons lns = S.size $ go S.empty lns
    where
        go acc []      = acc
        go acc [_]     = acc
        go acc (l:lns) = go acc' lns
            where acc' = foldl' (\s l' -> S.union s $ commonPoints l l') acc lns


solve1 :: [Line] -> Int
solve1 = countCommons . filter isHV

solve2 :: [Line] -> Int
solve2 = countCommons

parse :: String -> Line
parse = mkLine . map read . words . map go
    where go c = if c==',' || c=='-' || c=='>' then ' ' else c

main :: IO ()
main = do
    lns <- map parse . lines <$> readFile  "day_5_input.txt"
    putStrLn $ "#1: " ++ show (solve1 lns)
    putStrLn $ "#2: " ++ show (solve2 lns)
