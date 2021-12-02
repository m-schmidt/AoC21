-- https://adventofcode.com/2021/day/1

import Data.List


-- 1 Counting changes of depth
data DepthChange = None | Increase | Decrease deriving Eq

depthChanges :: [Int] -> [DepthChange]
depthChanges []     = [None]
depthChanges (x:xs) = snd $ mapAccumL go x xs
    where
        go acc d = (d, cmp acc d)
        cmp a b | a < b = Increase
        cmp a b | a > b = Decrease
        cmp _ _         = None

countIncreases :: [DepthChange] -> Int
countIncreases = length . filter (== Increase)


-- 2 ...with a sliding window of fixed size
data Window a = Wnd Int [a] deriving Show

empty :: Window a
empty = Wnd 0 []

-- Append a new element to a window
append :: Int -> Window a -> a -> Window a
append 1 _ x = Wnd 1 [x]
append n (Wnd cnt xs) x | cnt < n = Wnd (cnt+1) (x:xs)
append n (Wnd _   xs) x           = Wnd n (x : take (n-1) xs)

-- Filter windows grown to size [n]
valid :: Int -> Window a -> Bool
valid n (Wnd m _) = n == m

-- Conversion of window content to final values
finalizeWith :: ([a] -> b) -> Window a -> b
finalizeWith f (Wnd _ xs) = f xs

-- Slide a fixed sized window over a list of elements and sum the window elements
slidingWindow :: Num a => Int -> [a] -> [a]
slidingWindow 1 xs         = xs
slidingWindow n xs | n > 1 = map (finalizeWith sum) $ filter (valid n) $ scanl (append n) empty xs
slidingWindow _ _          = undefined


main :: IO ()
main = do
    depths <- map read . lines <$> readFile "day_1_input.txt"
    putStrLn $ "#1: " ++ show (countIncreases $ depthChanges depths)
    putStrLn $ "#2: " ++ show (countIncreases $ depthChanges $ slidingWindow 3 depths)
