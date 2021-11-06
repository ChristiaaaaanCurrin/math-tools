{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE FlexibleInstances     #-}

module Statics where

import Vectors
import Matrices

class Body b a where
  equilibrium :: b -> Matrix a

data Point a = Point (Vector a) [Vector a] [Vector a] deriving (Read, Show, Eq)
--                    Position   Forces     Moments
data Member a = Member [Point a] [Point a] deriving (Show, Read, Eq)
--                      Unknowns  Knowns
data Frame a = Frame (Member a) | Join (Point a) (Member a) (Member a)

o = [0, 0, 0]
i = [1, 0, 0]
j = [0, 1, 0]
k = [0, 0, 1]

memF u (Member us ks) = [dot u $         unit f | Point p fs ms <- us, f <- fs]
       ++ [negate $ sum [dot u                f | Point p fs ms <- ks, f <- fs]]
memM u (Member us ks) = [dot u $ crs p $ unit f | Point p fs ms <- us, f <- fs]
                     ++ [dot u $         unit m | Point p fs ms <- us, m <- ms]
     ++ [negate $ sum $ [dot u $ crs p        f | Point p fs ms <- ks, f <- fs]
                     ++ [dot u                m | Point p fs ms <- ks, m <- ms]]

instance Body (Member Double) Double where
  equilibrium mem = [f u mem | u <- [i, j, k], f <- [memF, memM]]

instance Body (Frame Double) Double where
  equilibrium (Frame mem) = equilibrium mem
  equilibrium (Join uj mem1 mem2) =           head                [ij  ++ aj  ++ ojj,
                                                                   ij  ++ ojj ++ aj,
                                                                   o1j ++ a1  ++ o12,
                                                                   o2j ++ o21 ++ a2,
                                                                   [bj ++ b1  ++ b2]] where
    a1 = map init $ equilibrium mem1
    a2 = map init $ equilibrium mem2
    aj = map init $ equilibrium $ Member [uj] []
    b1 = map last $ equilibrium mem1
    b2 = map last $ equilibrium mem2
    bj = map last $ equilibrium $ Member [uj] []
    d1 = length $ head a1
    d2 = length $ head a2
    dj = length $ head aj
    ij = identity dj
    o12 = zeros d1 6
    o21 = zeros d2 6
    ojj = zeros dj 6
    o1j = zeros d1 dj
    o2j = zeros d2 dj

equilibrium' :: Frame Double -> [Matrix Double]
equilibrium' (Join uj mem1 mem2) =                  map tsp       [ij  ++ aj  ++ ojj,
                                                                   ij  ++ ojj ++ aj,
                                                                   o1j ++ a1  ++ o12,
                                                                   o2j ++ o21 ++ a2,
                                                                   [bj ++ b1  ++ b2]] where
    a1 = map init $ equilibrium mem1
    a2 = map init $ equilibrium mem2
    aj = map init $ equilibrium $ Member [uj] []
    b1 = map last $ equilibrium mem1
    b2 = map last $ equilibrium mem2
    bj = map last $ equilibrium $ Member [uj] []
    d1 = length $ head a1
    d2 = length $ head a2
    dj = length $ head aj
    ij = identity dj
    o12 = zeros d1 6
    o21 = zeros d2 6
    ojj = zeros dj 6
    o1j = zeros d1 dj
    o2j = zeros d2 dj
