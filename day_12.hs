{-# LANGUAGE DeriveFunctor #-}
-- https://adventofcode.com/2021/day/12

import qualified Data.Set as S
import qualified Data.Map.Strict as M
import Recursion ( hylo, Algebra, Coalgebra )
import Data.Char (isUpper, isLower)


-- Visit log with set of visited nodes and flag indicating whether a second visit to a small cave is still allowed
data V = V (S.Set String) Bool

canVisit :: V -> String -> Bool
canVisit _ "start"                       = False
canVisit (V _ True) x | isLower $ head x = True
canVisit (V s    _) x                    = not (x `S.member` s)

visit :: String -> V -> V
visit x v | isUpper $ head x        = v
visit x (V s True) | x `S.member` s = V s False
visit x (V s o)                     = V (x `S.insert` s) o


data TreeF x = Leaf String
             | Node String [x]
             deriving Functor

-- Coalgebra to unfold a tree of paths with current cave and visit log as seed
data D = D String V

coa :: M.Map String (S.Set String) -> Coalgebra TreeF D
coa _  (D "end" _) = Leaf "end"
coa ni (D c v)     = Node c (map go ns)
    where
        ns   = filter (canVisit v) . S.elems $ ni M.! c
        go n = D n (visit n v)

-- Algebra to count valid paths through the caves
alg :: Algebra TreeF Int
alg (Leaf _)    = 1
alg (Node _ ts) = sum ts


-- Hylomorphism for the algorithm
solve :: M.Map String (S.Set String) -> Bool -> Int
solve plan f = hylo alg (coa plan) initial_d
    where
        initial_d = D "start" (V (S.singleton "start") f)


-- Parse input as map from cave to set of neighbored caves
parse :: String -> (String, String)
parse s = (head xs, last xs)
    where xs = words $ map (\c -> if c == '-' then ' ' else c) s

collect :: (String, String) -> M.Map String (S.Set String) -> M.Map String (S.Set String)
collect (a, b) = M.alter (upd b) a . M.alter (upd a) b
    where
        upd v Nothing  = Just (S.singleton v)
        upd v (Just s) = Just (v `S.insert` s)


main :: IO ()
main = do
    plan <- foldr (collect . parse) M.empty . lines <$> readFile  "day_12_input.txt"
    putStrLn $ "#1: " ++ show (solve plan False)
    putStrLn $ "#2: " ++ show (solve plan True)
