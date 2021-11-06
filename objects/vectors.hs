
module Objects.Vectors where

type Vector a = [a]

rad2deg = (*) (180 / pi)
deg2rad = (*) (pi / 180)

mag :: Floating a => Vector a -> a
mag v   = sqrt $ sum $ map (^2) v

dot :: Num a => Vector a -> Vector a -> a
dot v u = sum $ zipWith (*) v u

add :: Num a => Vector a -> Vector a -> Vector a
add v u = zipWith (+) v u

sub v = (add v).(scl (-1))

crs :: Num a => Vector a -> Vector a -> Vector a
crs [x1, y1, z1] [x2, y2, z2] = [ y1 * z2 - y2 * z1 , z1 * x2 - z2 * x1 , x1 * y2 - y1 * x2 ]
crs [x1, y1] [x2, y2] = crs [x1, y1, 0] [x2, y2, 0]

aug = (++) [0]

ang :: Floating a => Vector a -> Vector a -> a
ang v u = acos $ (dot v u) / ((mag v) * (mag u))

scl :: Num a => a -> Vector a -> Vector a
scl = map . (*)

unit :: Floating a => Vector a -> Vector a
unit v = map (/ mag v) v

prj :: Floating a => Vector a -> Vector a -> Vector a
prj v u = scl (dot v u / mag u) $ unit u
{-
instance (Num a, Eq a) => Num [a] where
  v * u = crs v u
  v + u = add v u
  negate = map negate
  abs = map abs
  fromInteger = return . fromInteger
  signum = map signum
-}

