
{-# LANGUAGE MultiParamTypeClasses #-}

module Go where

import Objects.Graph
import Move
import Util
import Rule

data GoState g a p = GS (g a) (a -> p)
data GoMove a p = GM [(a, p)] deriving (Read, Show, Eq)

instance (Show a, Show p, FiniteGraph g, Eq a) => Show (GoState g a p) where
  show (GS g f) = (++) "GS" $ show $ map f $ vertices g
instance Eq a => Move (GoState g a p) (GoMove a p) where
  GM xs # GS g f = GS g (foldr replace f xs) 
instance Semigroup (GoMove a p) where
  GM xs <> GM ys = GM (xs ++ ys)

ban :: GoState g a p -> g a
ban (GS g f) = g

place :: (FiniteGraph g, Eq a, Eq p) => p -> [p] -> Rule (GoState g a p) (GoMove a p)
place l ps (GS g f) = [GM [(v, p)] | v <- vertices g, p <- ps, f v == l]

capture :: (FiniteGraph g, Eq a, Eq p) => p -> [p] -> Rule (GoState g a p)  (GoMove a p)
capture l ps (GS g f) = [GM [(v, l) | v <- vertices h] | p <- ps,
                                                       let h = subgraph g $ filter ((==p).f) $ vertices g,
                                                       null $ filter ((==l).f) $ concat $ map (neighborhood g) $ vertices h]



tgs :: GoState NeighborhoodGraph Int Char
tgs = GS (latice 0 1 [0..3]) (const '_')

c :: Rule (GoState NeighborhoodGraph Int Char) (GoMove Int Char)
c = capture '_' "wb"
p :: Rule (GoState NeighborhoodGraph Int Char) (GoMove Int Char)
p = place '_' "wb" 
go = place '_' "wb"
