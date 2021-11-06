{-# LANGUAGE TypeSynonymInstances #-}
{-# LANGUAGE FlexibleInstances #-}

module Objects.Matrices where

import Objects.Vectors

type Matrix a = [Vector a]

vanish d = map $ map $ (/d).fromIntegral.round.(*d)

dim :: Matrix a -> (Int, Int)
dim [] = (0, 0)
dim m = (length m, length $ m!!0)

identity :: Num b => Int -> Matrix b
identity d = [take d $ (take x $ repeat 0) ++ [1] ++ (repeat 0) | x <- [0..(d-1)]]

zeros    :: Num b => Int -> Int -> Matrix b
zeros m n  = [[fromInteger 0 | x <- [1..m]] | y <- [1..n]]

tsp :: Matrix a -> Matrix a
tsp = foldr (zipWith (++)) (repeat []) . map (map return)

mvm :: Num a => Matrix a -> Vector a -> Vector a
mvm = flip (map . dot)

mmm :: Num a => Matrix a -> Matrix a -> Matrix a
mmm = ((tsp.).(.tsp)).(map.mvm)
--mmm m1 m2 = [mvm m1 v | v <- tsp  m2]

rot :: Floating a => a -> Vector a -> Vector a
rot a = mvm [[cos a, -sin a],[sin a, cos a]]

rowReplace :: Int -> Matrix a -> Vector a -> Matrix a
rowReplace r m v = (++) (take r m) $ (:) v $ drop (r+1) m

rowRemove :: Int -> Matrix a -> Matrix a
rowRemove r m = (++) (take r m) $ drop (r+1) m

rowInsert :: Int -> Matrix a -> Vector a -> Matrix a
rowInsert r m v = (++) (take r m) $ (:) v $ drop r m

rowMult :: Num a => Int -> a -> Matrix a -> Matrix a
rowMult r s m = rowReplace r m $ scl s (m!!r)

rowAdd :: Num a => Int -> Int -> Matrix a -> Matrix a
rowAdd r1 r2 m = rowReplace r1 m $ add (m!!r1) (m!!r2)

rowSwap :: Int -> Int -> Matrix a -> Matrix a
rowSwap r1 r2 m = rowReplace r1 (rowReplace r2 m (m!!r1)) (m!!r2)

scaleRow :: (Eq a, Fractional a) => Int -> Int -> Matrix a -> Matrix a
scaleRow r c m = rowMult r (recip $ m!!r!!c) m

cancel :: (Fractional a) => Int -> Int -> Int -> Matrix a -> Matrix a
cancel r1 r2 c m = rowAdd r1 r2 $ rowMult r1 ((/) (negate $ m!!r2!!c) $ m!!r1!!c) m

stepCancel :: (Eq a, Fractional a) => Int -> Int -> Int -> Matrix a -> Matrix a
stepCancel r1 r2 c m
  | r1 == r2 = stepCancel (r1+1) r2 c m
  | r1 >= (fst $ dim m) = m 
  | (m!!r1!!c) == 0 = stepCancel (r1+1) r2 c m
  | otherwise = stepCancel (r1+1) r2 c $ cancel r1 r2 c m

cancelAll :: (Eq a, Fractional a) => Int -> Int -> Matrix a -> Matrix a
cancelAll = stepCancel 0

stepRREF :: (Eq a, Fractional a) => Int -> Int -> [Int] -> Matrix a -> Matrix a
stepRREF r c rs m
  | c >= (uncurry min $ dim m) = m
  | r >= (fst $ dim m) = stepRREF 0 (c+1) rs m
  | (m!!r!!c) == 0 || r `elem` rs = stepRREF (r+1) c rs m
  | otherwise = scaleRow r c $ stepRREF 0 (c+1) (r:rs) $ cancelAll r c m

rref :: (Eq a, Fractional a) => Matrix a -> Matrix a
rref = stepRREF 0 0 []

stepREF :: (Eq a, Fractional a) => Int -> Int -> [Int] -> Matrix a -> Matrix a
stepREF r c rs m
  | c >= (uncurry min $ dim m) = m
  | r >= (fst $ dim m) = stepREF 0 (c+1) rs m
  | (m!!r!!c) == 0 || r `elem` rs = stepREF (r+1) c rs m
  | otherwise = stepREF 0 (c+1) (r:rs) $ stepCancel r r c m

ref :: (Eq a, Fractional a) => Matrix a -> Matrix a
ref = stepREF 0 0 []

ringCancel r1 r2 c m = rowAdd r1 r2 $ rowMult r1 (negate $ m!!r2!!c) $ rowMult r2 (m!!r1!!c) m

stepRingCancel :: (Eq a, Fractional a) => Int -> Int -> Int -> Matrix a -> Matrix a
stepRingCancel r1 r2 c m
  | r1 == r2 = stepRingCancel (r1+1) r2 c m
  | r1 >= (fst $ dim m) = m 
  | (m!!r1!!c) == 0 = stepRingCancel (r1+1) r2 c m
  | otherwise = stepRingCancel (r1+1) r2 c $ ringCancel r1 r2 c m

stepRingREF :: (Eq a, Fractional a) => Int -> Int -> [Int] -> Matrix a -> Matrix a
stepRingREF r c rs m
  | c >= (uncurry min $ dim m) = m
  | r >= (fst $ dim m) = stepRingREF 0 (c+1) rs m
  | (m!!r!!c) == 0 || r `elem` rs = stepRingREF (r+1) c rs m
  | otherwise = stepRingREF 0 (c+1) (r:rs) $ stepRingCancel r r c m

ringREF :: (Eq a, Fractional a) => Matrix a -> Matrix a
ringREF = stepRingREF 0 0 []


inv :: (Eq a, Fractional a) => Matrix a -> Matrix a
inv m = map (drop $ length m) $ rref $ zipWith (++) m (identity $ length m)

adj m = reverse $ ref $ reverse $ ref m
--adj m = map (drop $ length m) $ ref $ zipWith (++) m (identity $ length m)
inv' m = mmm (sclm (length m) (det m)) $ adj m

invertible :: (Eq a, Fractional a) => Matrix a -> Bool
invertible m = (&&) (fst (dim m) == snd (dim m))
             $ (==1) $ last $ last $ map (take $ length m) $ rref $ zipWith (++) m (identity $ length m)

leastSquares :: (Eq a, Fractional a) => Matrix a -> Vector a -> Matrix a
leastSquares a y = rref $ zipWith (++) (mmm (tsp a) a) $ map pure $ mvm (tsp a) y

cofactors :: Matrix a -> [Matrix a]
cofactors [v] = []
cofactors x = map (tail.tsp) [drop (n+1) a ++ take n a | (n, v) <- zip [0..] a] where a = tsp x

det :: Num a => Matrix a -> a
det [v] = product v
det [v, u] = sum $ add (scl (head v) (tail u)) (scl (negate $ head u) (tail v))
det a = sum $ zipWith (*) (head a) (map det $ cofactors a)

diag :: Num a => Vector a -> Matrix a
diag v = zipWith scl v $ identity $ length v

sclm :: Num a => Int -> a -> Matrix a
sclm n = diag.take n.repeat

instance (Eq a, Num a, Fractional a) => Num (Matrix a) where
  (*) = mmm
  (+) = zipWith (add)
  negate = map $ map negate
  abs a  = mmm a $ inv $ map (map signum) a 
  signum = map $ map signum
  --fromInteger i = ((fromInteger i):(repeat 0)):(map (0:)  $ fromInteger i)
  fromInteger = identity.fromInteger.toInteger

instance (Eq a, Fractional a) => Fractional (Matrix a) where
  recip = inv
  fromRational r = [[fromRational r]]

ortho :: (Num a, Eq a, Fractional a) => Matrix a -> Vector a -> Vector a
ortho m v = sub v $ mvm (tsp m) $ map last $ leastSquares (tsp m) v

gramSchmidt [] = []
gramSchmidt (v:[]) = [unit v]
gramSchmidt (v:vs) = (unit $ ortho (gramSchmidt vs) v):(gramSchmidt vs)

qrq = tsp.gramSchmidt.tsp
qrr a = mmm (tsp $ qrq a) a


