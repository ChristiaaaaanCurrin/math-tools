
module Manifold where

import Group
import Data.List (nubBy)
import Util (without)
import Graph

class Manifold m where
  eulerCharacteristic :: m -> Int
  orientable :: m -> Bool

data Surface = P2 Int | T2 Int | S2 deriving (Show, Read, Eq) 
instance Manifold Surface where
  eulerCharacteristic (P2 n) = 2 - n
  eulerCharacteristic (T2 n) = 2 - 2 * n
  eulerCharacteristic S2 = 2
  orientable (P2 _) = False 
  orientable m      = True  
instance Semigroup Surface where
  S2 <> x = x
  T2 n <> T2 m = T2 (n + m)
  P2 n <> P2 m = P2 (n + m)
  P2 n <> T2 m = P2 (n + 2 * m) 
  m1 <> m2 = m2 <> m1
instance Monoid Surface where
  mempty = S2

classify :: (Group g, Eq g) => [g] -> Surface
classify [] = S2
classify [_] = S2
classify es

