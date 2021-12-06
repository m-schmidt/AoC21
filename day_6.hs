-- https://adventofcode.com/2021/day/6

import qualified Data.Map.Strict as M

toMap :: [Int] -> M.Map Int Int
toMap = foldr (M.alter f) emptyAges
    where
        emptyAges  = M.fromList $ zip [0..8] (repeat 0)
        f (Just n) = Just (n+1)
        f Nothing  = undefined

simulate :: M.Map Int Int -> M.Map Int Int
simulate ages = M.mapWithKey f ages
    where
        f 6 _ = ages M.! 0 + ages M.! 7
        f 8 _ = ages M.! 0
        f n _ = ages M.! (n+1)

count :: Int -> M.Map Int Int -> Int
count n ages = sum . M.elems $ iterate simulate ages !! n

parse :: String -> [Int]
parse = map read . words . map go
    where go c = if c==',' then ' ' else c

main :: IO ()
main = do
    ages <- toMap . parse <$> readFile  "day_6_input.txt"
    putStrLn $ "#1: " ++ show (count 80 ages)
    putStrLn $ "#2: " ++ show (count 256 ages)
