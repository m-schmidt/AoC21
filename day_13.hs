-- https://adventofcode.com/2021/day/13

import Data.List ( foldl' )
import qualified Data.Set as S


foldHV :: ((Int, Int) -> Bool) -> ((Int, Int) -> (Int, Int)) -> S.Set (Int, Int) -> S.Set (Int, Int)
foldHV pred trans sh = S.union pk pt
    where
        (pk, pf) = S.partition pred sh
        pt       = S.map trans pf

foldVertical :: Int -> S.Set (Int, Int) -> S.Set (Int, Int)
foldVertical pos = foldHV p t
    where
        p (x, _) = x <= pos
        t (x, y) = (2*pos - x, y)

foldHorizontal :: Int -> S.Set (Int, Int) -> S.Set (Int, Int)
foldHorizontal pos = foldHV p t
    where
        p (_, y) = y <= pos
        t (x, y) = (x, 2*pos - y)

applyFolds :: [(String, Int)] -> S.Set (Int, Int) -> S.Set (Int, Int)
applyFolds fs sh = foldl' go sh fs
    where
        go sh ("x", n) = foldVertical n sh
        go sh ("y", n) = foldHorizontal n sh
        go _ _ = undefined


splitOn :: Char -> String -> [String]
splitOn ch s = words $ map (\c -> if c == ch then ' ' else c) s

parsePoint :: String -> (Int, Int)
parsePoint s = (x,y)
    where [x,y] = map read $ splitOn ',' s

parseFold :: String -> (String, Int)
parseFold s = (hv, read pos)
    where [hv,pos] = splitOn '=' $ drop (length "fold along ") s

parse :: [String] -> (S.Set (Int, Int), [(String, Int)])
parse lns = (sheet, folds)
    where
        sheet = S.fromList $ map parsePoint $ filter (elem ',') lns
        folds = map parseFold $ filter (elem '=') lns


solve1 :: S.Set (Int, Int) -> [(String, Int)] -> Int
solve1 sh fs = S.size $ applyFolds (take 1 fs) sh

solve2 :: S.Set (Int, Int) -> [(String, Int)] -> String
solve2 sh fs =
    unlines [ [ c | x <- [0..w], let c = if S.member (x, y) sh' then '#' else '.' ] | y <- [0..h] ]
    where
        sh'    = applyFolds fs sh
        (w, h) = S.foldl' (\(w,h) (x,z) -> (max w x, max h z)) (0, 0) sh'


main :: IO ()
main = do
    (sheet, folds) <- parse . lines <$> readFile  "day_13_input.txt"
    putStrLn $ "#1: " ++ show (solve1 sheet folds)
    putStrLn $ "#2:\n" ++ solve2 sheet folds
