-- https://adventofcode.com/2021/day/6

import qualified Data.Map.Strict as M


initAges :: [Int] -> M.Map Int Int
initAges = foldr (M.alter f) emptyAges
    where
        emptyAges  = M.fromList $ zip [0..8] (repeat 0)
        f (Just n) = Just (n+1)
        f Nothing  = undefined

updateAges :: M.Map Int Int -> M.Map Int Int
updateAges ages = M.mapWithKey f ages
    where
        get k = M.findWithDefault 0 k ages
        f 6 _ = get 0 + get 7
        f 8 _ = get 0 + get 9
        f n _ = get (n+1)

count :: Int -> M.Map Int Int -> Int
count n ages = sum . M.elems $ iterate updateAges ages !! n

parse :: String -> [Int]
parse = map read . words . map go
    where go c = if c==',' then ' ' else c

main :: IO ()
main = do
    ages <- initAges. parse <$> readFile  "day_6_input.txt"
    putStrLn $ "#1: " ++ show (count 80 ages)
    putStrLn $ "#2: " ++ show (count 256 ages)
