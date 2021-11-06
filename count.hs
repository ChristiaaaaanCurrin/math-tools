
module Count where
import Objects.Matrices

--thank ghc for tail recursion
factorial 0 = 1
factorial n = n * factorial (n - 1)

falling n m
  | m * n > 0 = m * falling (n - 1) (m - 1)
  | otherwise = 1

rising n m
  | n > 0 = m * rising (n - 1) (m + 1)
  | otherwise = 1

--an old classic
binomial n k = falling k n `div` factorial (k)  
--no real reason
pasc _ 0 = 1
pasc 0 _ = 0
pasc n k = pasc (n-1) (k-1) + pasc (n-1) k

--trust in the formulae
sab1 n k = binomial (n - 1) (k - 1)
sab2 n k = binomial (k + n - 1) (k - 1)

--trees did this one
partition :: (Integral a) => a -> a
partition 0 = 1
partition n = let b = (1 + sqrt (24 * fromIntegral n + 1)) / 6
              in -sum [(-1)^abs k * partition (n - k * (3 * k - 1) `div` 2) | k <- [ceiling (-b) .. floor b], k /= 0]

--ha! original creation
stir2 n k = sum [(-1)^i * binomial k i * (k - i)^n | i <- [0..k]] `div` factorial k
 
--needs an understanding check
derange n = sum [(-1)^(i-1) * falling n (n - i) | i <- [1..n]]

polyterm xs ys r = sum [xs!!i * ys!!(r-i) | i <- [0..r]]

--more understanding needed...
catalan n = binomial (2 * n) n `div` (1 + n)


