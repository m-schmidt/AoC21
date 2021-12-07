-- https://adventofcode.com/2021/day/7

parse :: String -> [Int]
parse = map read . words . map go
    where go c = if c==',' then ' ' else c

-- 1: cost is pure distance
cost1 :: Int -> Int
cost1 = id

-- 2: cost defined by Gauss formula for sum of first n numbers
cost2 :: Int -> Int
cost2 n = n * (n+1) `div` 2

solve :: (Int -> Int) -> [Int] -> Int
solve cost ps = minimum $ map go [minimum ps .. maximum ps]
    where
        go t = foldr (\p acc -> acc + cost (abs (p-t))) 0 ps

main :: IO ()
main = do
    positions <- parse <$> readFile  "day_7_input.txt"
    putStrLn $ "#1: " ++ show (solve cost1 positions)
    putStrLn $ "#2: " ++ show (solve cost2 positions)
