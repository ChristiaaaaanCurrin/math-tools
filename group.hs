
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE FlexibleInstances     #-}
{-# LANGUAGE UndecidableInstances  #-}
{-# LANGUAGE AllowAmbiguousTypes   #-}

module Group where

import Move
import Data.Semigroup

class Monoid m => Group m where
  inv  :: m -> m

instance (Group b1, Group b2) => Group (b1, b2) where 
  inv  (y1, y2) = (inv y1, inv y2)
instance (Group b1, Group b2, Group b3) => Group (b1, b2, b3) where 
  inv  (y1, y2, y3) = (inv y1, inv y2, inv y3)
instance (Group b1, Group b2, Group b3, Group  b4) => Group (b1, b2, b3, b4) where 
  inv  (y1, y2, y3, y4) = (inv y1, inv y2, inv y3, inv y4)
instance (Group b1, Group b2, Group b3, Group b4, Group b5) => Group (b1, b2, b3, b4, b5) where 
  inv  (y1, y2, y3, y4, y5) = (inv y1, inv y2, inv y3, inv y4, inv y5)

instance (Monoid b, Monoid c) => Monoid (Either b c) where
  mempty = Left  mempty
instance (Group b, Group c) => Group (Either b c) where
  inv  (Left  y) = Left  $ inv  y
  inv  (Right y) = Right $ inv  y

instance (Group b) => Group [b] where
  inv  ys = map inv $ reverse ys

instance (Semigroup b) => Semigroup (Select b) where
  (<>) (Select is y1) (Select js y2) = Select is (y1 <> y2)
instance (Monoid b) => Monoid (Select b) where
  mempty = Select [0] mempty
instance (Group b) => Group (Select b) where
  inv (Select is y) = Select is (inv y)

data Perm = S [Int] | SE deriving (Show, Read)
instance Eq Perm where
  SE   == S xs = all id $ zipWith (==) xs [0..]
  S xs == SE   = all id $ zipWith (==) xs [0..]
  S xs == S ys = all id $ zipWith (==) xs ys
instance Semigroup Perm where
  SE <> x  = x
  x  <> SE = x
  (S xs) <> (S ys) = S $ map (xs!!) ys 
instance Monoid Perm where
  mempty = SE
instance Group Perm where
  inv (S xs)
     | S xs == SE = SE
     | SE == S xs = SE
     | otherwise  = S [m | (n, y) <- zip [0..] xs, (m, x) <- zip [0..] xs, n == x]
instance Move Perm Perm where
  (#) = (<>)

generate x = (x <> inv x) : (takeWhile (/= mempty) $ iterate (<> x) x)

