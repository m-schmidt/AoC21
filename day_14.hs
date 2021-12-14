-- https://adventofcode.com/2021/day/14

import Data.List ( sort )
import Data.Maybe ( mapMaybe )
import qualified Data.Map.Strict as M


-- Map charcater pair to inserted character
type Insertions = M.Map (Char, Char) Char

-- Map character pair to number of occurences
type Polymer = M.Map (Char, Char) Int


increment :: Ord k => Int -> k -> M.Map k Int -> M.Map k Int
increment n = M.alter (incMaybe n)
    where
        incMaybe n Nothing  = Just n
        incMaybe n (Just m) = Just $ n+m


step :: Insertions -> Polymer -> Polymer
step i = M.foldrWithKey go M.empty
    where
        go (a, b) cnt acc = case i M.!? (a, b) of
            Nothing -> increment cnt (a, b) acc
            Just c  -> increment cnt (a, c) $ increment cnt (c, b) acc

steps :: Insertions -> Int -> Polymer -> Polymer
steps t n x = iterate (step t) x !! n

counts :: Polymer -> [(Char, Int)]
counts = mapMaybe finish . M.toList . M.foldrWithKey go M.empty
    where
        go (a, b) cnt acc = increment cnt a $ increment cnt b acc
        finish (c, n)     = let n' = n `div` 2 in if n' /= 0 then Just (c, n') else Nothing

solve :: Insertions -> Int -> Polymer -> Int
solve t n s = head cnts - last cnts
    where
        cnts = reverse $ sort $ map snd $ counts $ steps t n s


parsePolymer :: String -> Polymer
parsePolymer s = foldr (increment 1) M.empty $ zip ('[':s) (s ++ "]")

parseInsertion :: String -> ((Char, Char), Char)
parseInsertion s = ((a, b), c)
    where
        a = head s
        b = s !! 1
        c = last s

parse :: [String] -> (Polymer, Insertions)
parse lns = (polymer, insertions)
    where
        polymer    = parsePolymer $ head lns
        insertions = M.fromList $ map parseInsertion $ drop 2 lns


main :: IO ()
main = do
    (polymer, insertions) <- parse . lines <$> readFile "day_14_input.txt"
    putStrLn $ "#1: " ++ show (solve insertions 10 polymer)
    putStrLn $ "#2: " ++ show (solve insertions 40 polymer)
