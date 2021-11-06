
module Sudoku where

import Graph
import Util
import Data.List hiding (delete, intersect) 

rows      n = WG [[i..i+n-1] | i <- [0,n..n^2-1]]
columns   n = WG [map (+i) [0,n..n^2-1] | i <- [0..n-1]]
grid      n = join (rows n) (columns n)
blocks    n = WG $ concat [[concat [[0+i+j+k..n-1+i+j+k] | i <- [0, n^2..n^2*(n-1)]] | j <- [0,n..n* (n-1)]] | k <- [0,n^3..n^3*(n-1)]]
--blockgrid n = let m = n^2 in foldr join (blocks n) [rows m, columns m]

row     x y = fst x == fst y && x /= y 
column  x y = snd x == snd y && x /= y
block n x y = fst x `div` n == fst y `div` n && snd x `div` n == snd y `div` n && x /= y

bgg n = let vs = [(i,j) | i <- [0..n^2-1], j <- [0..n^2-1]] in RG vs row `join` RG vs column `join` RG vs (block n)


doku :: (FiniteGraph g, Eq v, Eq a) => g v -> (v -> [a]) -> (v -> [a])
doku g f
  | null ps = f
  | otherwise = doku g $ plant (head ps) f
  where ps = [(u, f u \\ as) | v <- vertices g,
                               vs <- powerset $ neighborhood g v,
                               clique g vs,
                               not $ null vs,
                               let as = foldr (/\) (f $ head vs) $ map f vs,
                               length vs == length as,
                               u <- (foldr (/\) (vertices g) $ map (neighborhood g) vs) \\ vs,
                               not $ null $ f u /\ as]

kaku :: (FiniteGraph g, Eq v, Eq a) => [a] -> g v -> (v -> [a]) -> (v -> [a])
kaku as g f = foldr plant f [(u, cs) | v <- vertices g,
                                       vs <- powerset $ neighborhood g v,
                                       length vs == length as,
                                       cs <- powerset as,
                                       let us = filter ((flip all cs . flip elem).f) vs,
                                       length us == length cs,
                                       u <- us]

testg  = bgg 3
testf  = foldr plant (const [0..8]) [((0,0), [1]), ((0,1), [2])] 
testf' = doku testg testf
testf'' = kaku [0..8] testg testf'
