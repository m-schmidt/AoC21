-- https://adventofcode.com/2021/day/2

import Data.List


-- 1 Tracking position/depth
data Direction = Forward | Down | Up

parseDirection :: String -> Direction
parseDirection s = case s of
    "forward" -> Forward
    "down"    -> Down
    "up"      -> Up
    _         -> undefined

parseInt :: String -> Int
parseInt = read

parseCommand :: String -> (Direction, Int)
parseCommand s = (parseDirection d, parseInt i)
    where (d:i:_) = words s

exec :: (Int, Int) -> (Direction, Int) -> (Int, Int)
exec (h, d) (Forward, n) = (h+n, d)
exec (h, d) (Down, n)    = (h,   d+n)
exec (h, d) (Up, n)      = (h,   d-n)

run :: [(Direction, Int)] -> (Int, Int)
run = foldl exec (0, 0)


-- 2 Tracking with aim

execAim :: (Int, Int, Int) -> (Direction, Int) -> (Int, Int, Int)
execAim (h, d, a) (Forward, n) = (h+n, d+a*n, a)
execAim (h, d, a) (Down, n)    = (h,   d,     a+n)
execAim (h, d, a) (Up, n)      = (h,   d,     a-n)

runAim :: [(Direction, Int)] -> (Int, Int, Int)
runAim = foldl execAim (0, 0, 0)


main = do
    commands <- map parseCommand . lines <$> readFile "day_2_input.txt"
    let (h, d) = run commands
    putStrLn $ "#1: " ++ show (h*d)
    let (h, d, a) = runAim commands
    putStrLn $ "#2: " ++ show (h*d)
