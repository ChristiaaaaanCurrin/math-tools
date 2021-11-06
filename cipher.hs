module Cipher where

import Data.List
import Data.Maybe
import Objects.Matrices
import Objects.Vectors
import Numbers.Modular
import Numbers.Euclidean 
import Control.Applicative (liftA2)

char2mod :: Char -> Modulo Int 
char2mod = Mod 26.flip (-) 97.fromEnum

mod2char :: Modulo Int -> Char
mod2char = toEnum.(+ 97).fromJust.residue

affine    a b = map mod2char . affine' a b . map char2mod 
affine'   a b = map (\x -> a *  x + b)
unaffine  a b = map mod2char . unaffine' a b . map char2mod
unaffine' a b = map (\x -> (x - b) / a) 

vigenere key = map mod2char . vigenere' key . map char2mod
vigenere' key = zipWith (+) (cycle $ map char2mod key) 
unvigenere key = map mod2char . unvigenere' key . map char2mod
unvigenere' key = zipWith (flip (-)) $ cycle $ map char2mod key

hill' :: Num a => Matrix a -> Vector a -> Vector a
hill' k m 
  | null m = [] 
  | otherwise = head (mmm [take (length k) m] k) ++ hill' k (drop (length k) m)
hill k = map mod2char . hill' k . map char2mod

midentity m = map (map (Mod m)) . identity
mdiag m v = zipWith scl v $ midentity m $ length v
msclm m n = mdiag m.take n.repeat

minv m k = mmm (msclm m (length k) $ (recip $ det k)) $ ref k

base n 0 = []
base n m = let k = head [x | x <- [1..], n^x > m] - 1 in k : base n (m - n^k)

--unhill' k m = hill' (miv 26 k) m
--unhill k = map mod2char . unhill' k . map char2mod

fromContinuedFraction [x] = fromInteger $ toInteger x 
fromContinuedFraction (x:xs) = (fromInteger $ toInteger x) + (recip $ fromContinuedFraction xs)

continuedFraction x
  | floor x == ceiling x = pure $ floor x
  | otherwise = (:) (floor x) $ continuedFraction $ recip $ x - fromIntegral (floor x)

convergent :: (RealFrac a) => Int -> a -> a
convergent i = fromContinuedFraction . take i . continuedFraction

convergents r = map (flip convergent r) [1..length $ continuedFraction r]

fermat n = [(a - x, a + x) | x <- [0..n `div` 2], let a = round (sqrt $ fromInteger (n + x^2)), a^2 == n + x^2]
fermat' n = [(a - x, a + x, a^2 == n + x^2) | x <- [0..n `div` 2], let a = round (sqrt $ fromInteger (n + x^2))]

elgamal p a b k m = ((a%%p)^k, (b%%p)^k * m%%p)
unelgamal a (r, t) = t / r^a
