-- https://adventofcode.com/2021/day/11

import Data.List ( mapAccumL )
import Data.Maybe ( catMaybes )
import Data.Char ( digitToInt )


-- Energy level of a squid
data E = Energy Int | Flashed deriving Eq

instance Show E where
    show (Energy n) = show n
    show Flashed    = "."

incE :: E -> E
incE (Energy n) = Energy (n+1)
incE Flashed    = Flashed

resetE :: E -> E
resetE Flashed = Energy 0
resetE s       = s


-- Energy Matrix and basic operations
data M = Matrix Int [E]

at :: M -> (Int, Int) -> E
at (Matrix w m) (x, y) = m !! (y*w + x)

update :: (E -> E) -> (Int, Int) -> M -> M
update f (x, y) (Matrix w m) = Matrix w m'
    where
        m' = zipWith go [0..] m
        go n e | n == y*w + x = f e
        go _ e = e

raise :: (Int, Int) -> M -> M
raise = update incE

toFlashed :: (Int, Int) -> M -> M
toFlashed = update (const Flashed)

neighbors :: M -> (Int, Int) -> [(Int, Int)]
neighbors (Matrix w _) (x, y) = catMaybes
    [ pt (x-1) (y-1)
    , pt (x-1) y
    , pt (x-1) (y+1)
    , pt x     (y-1)
    , pt x     (y+1)
    , pt (x+1) (y-1)
    , pt (x+1) y
    , pt (x+1) (y+1) ]
    where
        pt x y | 0 <= x && x < w && 0 <= y && y < w = Just (x, y)
        pt _ _ = Nothing

increment :: M -> M
increment (Matrix w m) = Matrix w $ map incE m

propagate :: M -> M
propagate (Matrix w m) =
    case foldr upd ([], Matrix w m) ps of
        ([], m) -> m
        (fs, m) -> propagate $ foldr raise m $ concatMap (neighbors m) fs
    where
        ps = [ p | y <- [0..w-1], x <- [0..w-1], let p = (x, y) ]
        upd p (acc, m) =
            case at m p of
                Energy n | n > 9 -> (p:acc, toFlashed p m)
                _                -> (acc, m)

reset :: M -> (Int, M)
reset (Matrix w m) = (cnt, Matrix w m')
    where
        (cnt, m')   = mapAccumL r 0 m
        r n Flashed = (n+1, Energy 0)
        r n e       = (n, e)

step :: M -> (Int, M)
step = reset . propagate . increment


solve1 :: Int -> M -> Int
solve1 n m = fst $ foldr f (0,m) [1..n]
    where
        f _ (cnt, m) = case step m of
                           (cnt', m) -> (cnt+cnt', m)

solve2 :: M -> Int
solve2 m = go 1 m
    where
        go n m = case step m of
                    (100, _) -> n
                    (_,  m') -> go (n+1) m'


parse :: [String] -> M
parse l = Matrix (length l) (concatMap (map $ Energy . digitToInt) l)

main :: IO ()
main = do
    matrix <- parse . lines <$> readFile  "day_11_input.txt"
    putStrLn $ "#1: " ++ show (solve1 100 matrix)
    putStrLn $ "#2: " ++ show (solve2 matrix)
