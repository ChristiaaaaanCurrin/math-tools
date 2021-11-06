
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE FlexibleInstances     #-}
{-# LANGUAGE UndecidableInstances  #-}
{-# LANGUAGE AllowAmbiguousTypes   #-}

module Move where

infixr #
class Move a m where
  (#) :: m -> a -> a

instance (Move a1 b1, Move a2 b2) => Move (a1, a2) (b1, b2) where
  (#) (y1, y2) (x1, x2) = (y1 # x1, y2 # x2)

instance (Move a1 b1, Move a2 b2, Move a3 b3) => Move (a1, a2, a3) (b1, b2, b3) where
  (#) (y1, y2, y3) (x1, x2, x3) = (y1 # x1, y2 # x2, y3 # x3)

instance (Move a1 b1, Move a2 b2, Move a3 b3, Move a4 b4) => Move (a1, a2, a3, a4) (b1, b2, b3, b4) where
  (#) (y1, y2, y3, y4) (x1, x2, x3, x4) = (y1 # x1, y2 # x2, y3 # x3, y4 # x4)

instance (Move a1 b1, Move a2 b2, Move a3 b3, Move a4 b4, Move a5 b5) => Move (a1, a2, a3, a4, a5) (b1, b2, b3, b4, b5) where
  (#) (y1, y2, y3, y4, y5) (x1, x2, x3, x4, x5) = (y1 # x1, y2 # x2, y3 # x3, y4 # x4, y5 # x5)


instance (Move a b) => Move a [b] where
  (#) [] x = x
  (#) (y:ys) x = ys # y # x

instance (Move a b, Move a c) => Move a (Either b c) where
  (Left  y) # x = y # x
  (Right y) # x = y # x

data Select b = Select [Int] b deriving (Show, Read, Eq, Ord)
instance (Move a b) => Move [a] (Select b) where
  (#) (Select []     y) xs = xs 
  (#) (Select (i:is) y) xs = (Select is y) # (take i xs ++ [y # (xs!!i)] ++ drop (i+1) xs)

instance (Move a b) => Move (t -> a) (t -> b) where
  (m # s) x = (m x) # (s x) 

