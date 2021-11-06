module Numbers.Modular (
    Modulo(..),
    modulo, residue,
    (%%),
    allResidues, modElem
) where

import Numbers.Euclidean
import qualified Data.Ratio as Rat

-- Modulo a : Regular set of integers
-- Nomod    : {}
-- Unmod x  : {x}
-- Mod m r  : {r + m * k | forall integers k}
-- Nothing value for modulo represents an infinitely large modulo
data Modulo a = Nomod | Unmod a | Mod a a

modulo :: Modulo a -> Maybe a
modulo Nomod = Nothing
modulo (Unmod _) = Nothing
modulo (Mod m _) = Just m

residue :: Modulo a -> Maybe a
residue Nomod = Nothing
residue (Unmod r) = Just r
residue (Mod _ r) = Just r

infixl 8 %%
(%%) :: (Euclidean a) => a -> a -> Modulo a
r %% m = Mod m (r `emod` m)


-- Modified to make other things work well. How bad can it be?
instance (Eq a, Euclidean a) => Eq (Modulo a) where
    Nomod == Nomod = True
    Nomod == _ = False --added
    _ == Nomod = False --added
    x == y = residue x == residue y
    --Unmod r1 == Unmod r2 = r1 == r2
    --Mod m1 r1 == Mod m2 r2 = m1 == m2 && r1 `emod` m1 == r2 `emod` m2
    --_ == _ = False

instance (Show a) => Show (Modulo a) where
    showsPrec _ Nomod = ("Nomod" ++)
    showsPrec prec (Unmod r) =
        let s = "Unmod " ++ show r
        in (++) (if prec >= 10 then "(" ++ s ++ ")" else s)
    showsPrec prec (Mod m r) =
        let s = show r ++ " %% " ++ show m
        in (++) (if prec >= 8 then "(" ++ s ++ ")" else s)

instance (Eq a, Euclidean a) => Num (Modulo a) where
    Nomod + _ = Nomod
    _ + Nomod = Nomod
    Unmod r1 + Unmod r2 = Unmod (r1 + r2)
    Unmod r1 + Mod m r2 = Mod m $ (r1 + r2) `emod` m
    Mod m r1 + Unmod r2 = Mod m $ (r1 + r2) `emod` m
    Mod m1 r1 + Mod m2 r2 = let g = egcd m1 m2 in Mod g $ (r1 + r2) `emod` g
    
    Nomod * _ = Nomod
    _ * Nomod = Nomod
    Unmod r1 * Unmod r2 = Unmod (r1 * r2)
    Unmod 0 * Mod _ _ = Unmod 0
    Mod _ _ * Unmod 0 = Unmod 0
    Unmod r1 * Mod m r2 =
        let n = m * r1
        in Mod n $ (r1 * r2) `emod` n
    Mod m r1 * Unmod r2 =
        let n = m * r2
        in Mod n $ (r1 * r2) `emod` n
    Mod m1 r1 * Mod m2 r2 =
        let g = egcd m1 m2
            n = g * egcd (m1 `ediv` g) r1 * egcd (m2 `ediv` g) r2
        in Mod n $ (r1 * r2) `emod` n
    
    abs Nomod = Nomod
    abs (Unmod r) = Unmod (abs r)
    abs (Mod m r) = Unmod (egcd m r)
    
    signum Nomod = Nomod
    signum (Unmod r) = Unmod (signum r)
    signum (Mod m r) = let g = egcd r m in Mod (m `ediv` g) (r `ediv` g)
    
    fromInteger n = Unmod (fromInteger n)
    
    negate Nomod = Nomod
    negate (Unmod r) = Unmod (negate r)
    negate (Mod m r) = Mod m (negate r `emod` m)

instance (Eq a, Euclidean a) => Fractional (Modulo a) where
    recip Nomod = Nomod
    recip (Unmod _) = Nomod
    recip (Mod m r) =
        let (_, ri, g) = bezouts m r
        in if g /= 1
           then Nomod
           else Mod m ri
    
    fromRational _ = Nomod



allResidues :: (Enum a, Euclidean a) => a -> [Modulo a]
allResidues m = map (%% m) [0..pred m]

infix 4 `modElem`
modElem :: (Eq a, Euclidean a) => a -> Modulo a -> Bool
_ `modElem` Nomod = False
n `modElem` Unmod r = n == r
n `modElem` Mod m r = n `emod` m == r `emod` m



instance Functor Modulo where
    fmap _ Nomod = Nomod
    fmap f (Unmod r) = Unmod (f r)
    fmap f (Mod m r) = Mod (f m) (f r)
