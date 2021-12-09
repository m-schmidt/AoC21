-- https://adventofcode.com/2021/day/9

import Data.List
import Data.Char ( ord, digitToInt )
import Data.Maybe ( catMaybes )
import qualified Data.Set as S


data M a = M Int Int [a] deriving Show

at :: M a -> (Int, Int) -> a
at (M h w m) (x, y) = m !! (y * w + x)

neighbors :: M a -> (Int, Int) -> [(Int, Int)]
neighbors (M h w _) (x, y) = catMaybes [ pt (x-1) y, pt (x+1) y, pt x (y-1), pt x (y+1) ]
    where
        pt x y | 0 <= x && x < w && 0 <= y && y < h = Just (x, y)
        pt _ _ = Nothing

neighborValues :: M a -> (Int, Int) -> [a]
neighborValues m p = map (at m) $ neighbors m p

lowPoints :: M Int -> [(Int, Int)]
lowPoints m = case m of
    (M h w _) -> [ (x, y)
                 | y <- [0..h-1]
                 , x <- [0..w-1]
                 , let p = (x, y)
                 , let v = at m p
                 , all (> v) $ neighborValues m p]

solve1 :: M Int -> Int
solve1 m = sum . map ((+1) . at m) $ lowPoints m


basin :: M Int -> (Int, Int) -> S.Set (Int, Int)
basin m x = go S.empty [x]
    where
        go :: S.Set (Int, Int) -> [(Int, Int)] -> S.Set (Int, Int)
        go v [] = v
        go v (p:ps) | S.member p v = go v ps
        go v (p:ps) =
            let nb = [ p | p <- neighbors m p, not (S.member p v), at m p < 9 ] in
            go (S.insert p v) (nb ++ ps)

solve2 :: M Int -> Int
solve2 m = product $ take 3 $ reverse $ sort $ map (S.size . basin m) $ lowPoints m


parse :: [String] -> M Int
parse ss = M rows colums list
    where
        rows   = length ss
        colums = length $ head ss
        list   = concatMap (map digitToInt) ss

main :: IO ()
main = do
    matrix <- parse . lines <$> readFile  "day_9_input.txt"
    putStrLn $ "#1: " ++ show (solve1 matrix)
    putStrLn $ "#2: " ++ show (solve2 matrix)
