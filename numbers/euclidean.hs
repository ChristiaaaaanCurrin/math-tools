{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE UndecidableInstances #-}

module Numbers.Euclidean (
    Euclidean(..),
    egcd, elcm, bezouts,
    ERatio(), simplify,
    (%), numerator, denominator
) where

import Data.Int
import Data.Word
import qualified Data.Ratio as Rat

class (Num a) => Euclidean a where
    edivMod :: a -> a -> (a, a)
    
    ediv :: a -> a -> a
    emod :: a -> a -> a
    infixl 7 `ediv`
    infixl 7 `emod`
    
    edivMod x y = (ediv x y, emod x y)
    
    ediv x y = fst (edivMod x y)
    emod x y = snd (edivMod x y)
    
    {-# MINIMAL edivMod | ediv , emod #-}



egcd :: (Eq a, Euclidean a) => a -> a -> a
egcd x y = egcd' (abs x) (abs y)
    where egcd' a 0 = a
          egcd' a b = egcd' b (a `emod` b)

elcm :: (Eq a, Euclidean a) => a -> a -> a
elcm x y = (x * y) `ediv` egcd x y

-- Output is (x, y, gcd(a, b)) such that a * x + b * y = gcd(a, b)
bezouts :: (Eq a, Euclidean a) => a -> a -> (a, a, a)
bezouts a b = eea (1, 0, abs a) (0, 1, abs b)
    where eea xyr (_, _, 0) = xyr
          eea (x1, y1, r1) xyr@(x2, y2, r2) =
              let (q, r3) = edivMod r1 r2
              in eea xyr (x1 - x2 * q, y1 - y2 * q, r3)



data ERatio a = ERatio {numerator :: a, denominator :: a}

infix 7 %
(%) :: (Eq a, Euclidean a) => a -> a -> ERatio a
x % y = simplify (ERatio x y)

simplify :: (Eq a, Euclidean a) => ERatio a -> ERatio a
simplify (ERatio x y) = let g = egcd x y in ERatio (x `ediv` g) (y `ediv` g)


instance (Eq a, Num a) => Eq (ERatio a) where
    ERatio a b == ERatio c d = a * d == b * c

instance (Show a) => Show (ERatio a) where
    showsPrec prec (ERatio x y) =
        let s = show x ++ " % " ++ show y
        in (++) (if prec >= 7 then "(" ++ s ++ ")" else s)

instance (Eq a, Euclidean a) => Num (ERatio a) where
    ERatio a b + ERatio c d = simplify $ ERatio (a * d + b * c) (b * d)
    ERatio a b * ERatio c d = simplify $ ERatio (a * c) (b * d)
    
    abs (ERatio a b) = simplify $ ERatio (abs a) (abs b)
    signum (ERatio a b) = simplify $ ERatio (signum a) (signum b)
    fromInteger n = ERatio (fromInteger n) 1
    negate (ERatio a b) = ERatio (negate a) b

instance (Eq a, Euclidean a) => Fractional (ERatio a) where
    recip (ERatio a b) = ERatio b a 
    ERatio a b / ERatio c d = simplify $ ERatio (a * d) (b * c)
    fromRational r =
        let n = Rat.numerator r
            d = Rat.denominator r
        in ERatio (fromInteger n) (fromInteger d)

instance (Num a, Ord a) => Ord (ERatio a) where
  r <= s = numerator r * denominator s <= numerator s * denominator r

instance (Euclidean a, Real a) => Real (ERatio a) where
  toRational r = toRational (denominator r) / toRational (numerator r)

instance (Eq a, Euclidean a, Integral a) => RealFrac (ERatio a) where
  properFraction r = let a = numerator r; b = denominator r in (fromInteger $ toInteger $ a `div` b, (a `mod` b) % b)


instance Euclidean Int where
    edivMod = divMod

instance Euclidean Int8 where
    edivMod = divMod

instance Euclidean Int16 where
    edivMod = divMod

instance Euclidean Int32 where
    edivMod = divMod

instance Euclidean Int64 where
    edivMod = divMod

instance Euclidean Integer where
    edivMod = divMod

instance Euclidean Word where
    edivMod = divMod

instance Euclidean Word8 where
    edivMod = divMod

instance Euclidean Word16 where
    edivMod = divMod

instance Euclidean Word32 where
    edivMod = divMod

instance Euclidean Word64 where
    edivMod = divMod

