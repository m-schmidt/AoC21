-- https://adventofcode.com/2021/day/3

import Data.List
import Data.Maybe


-- 1 Gamma/epsilon rate
parseBitString :: String -> [Int]
parseBitString = mapMaybe parse
    where
        parse '1' = Just 1
        parse '0' = Just 0
        parse  _  = Nothing

toInt :: [Int] -> Int
toInt = foldl (\a b -> a*2 + b) 0

count :: [Int] -> Int
count bs = if sum bs > threshold then 1 else 0
    where
        -- threshold for most/least bits - roundes upwards in case number of input lines is odd
        threshold = (length bs + 1) `div` 2

-- 2 Life support rating
count' :: (Int -> Int -> Bool) -> [[Int]] -> [Int]
count' cmp [b] = b
count' cmp bss =
    let (bss0, bss1) = partition ([0] `isPrefixOf`) bss in
    let (bit, bss')  = if length bss0 `cmp` length bss1 then (1, bss1) else (0, bss0) in
    bit : count' cmp (map tail bss')

-- Oxygen: select 1, when there are fewer or same amount of 0s than 1s
countOxygen :: [[Int]] -> [Int]
countOxygen = count' (<=)

-- CO2: select 1, when there are more 0s than 1s
countCO2 :: [[Int]] -> [Int]
countCO2 = count' (>)


main :: IO ()
main = do
    bits <- map parseBitString . lines <$> readFile "day_3_input.txt"
    let bits' = transpose bits

    -- since no filtering per column is needed count on transposed bit strings
    let most    = count <$> bits'
    let gamma   = toInt most
    let epsilon = toInt $ map (1-) most
    putStrLn $ "#1: " ++ show (gamma * epsilon)

    -- count with filtering per column
    let oxygen =  toInt $ countOxygen bits
    let co2    =  toInt $ countCO2 bits
    putStrLn $ "#2: " ++ show (oxygen * co2)
