
module Util where

import qualified Data.List
import qualified Control.Applicative

xs /\ ys = Data.List.intersect xs ys
xs \/ ys = Data.List.union xs ys

powerset :: [a] -> [[a]]
powerset [] = [[]]
powerset (x:xs) = powerset xs ++ map (x:) (powerset xs)

without x = filter (/= x)

remove _ [] = []
remove x (y:xs) 
  | x == y = remove x xs
  | otherwise = y : remove x xs

replace :: Eq a => (a, b) -> (a -> b) -> a -> b
replace (x, y) f z
  | z == x = y
  | otherwise = f z

cart :: (Applicative f) => f a -> f b -> f (a, b)
cart = Control.Applicative.liftA2 (,)
