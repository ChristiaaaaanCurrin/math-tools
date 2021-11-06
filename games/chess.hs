
{-# LANGUAGE FlexibleContexts      #-}

module Chess where

import Nim 
import Rule
import Move
import Group

type ChessPieceType = Perm
type ChessBoard   a = (a, a) -> Bool
type ChessPiece p a = ((p, ChessPieceType), (a, a))
type ChessState p a = (p, [ChessPiece p a])

type MovePattern a = SymRule (a, a)

sqU :: Num a => MovePattern (Vec a)
sqU = const $ pure (0, 1)
sqD :: Num a => MovePattern (Vec a)
sqD = inv sqU
sqR :: Num a => MovePattern (Vec a)
sqR = const $ pure (1, 0)
sqL :: Num a => MovePattern (Vec a)
sqL = inv sqR

diagI :: Num a => MovePattern (Vec a)
diagI = sqU /. sqR
diagII :: Num a => MovePattern (Vec a)
diagII = sqU /. sqL
diagIII :: Num a => MovePattern (Vec a)
diagIII = inv diagI 
diagIV :: Num a => MovePattern (Vec a)
diagIV = inv diagII 

passBoard :: (Ord a, Num a, Group m) => Rule (ChessPiece p a, [ChessPiece p a]) m
passBoard = gate $ (\((t, (x, y)), xs) -> 0 < x && x <= 8 && 0 < y && y <= 8)
passFriendly :: (Eq a, Eq p, Group m) => Rule (ChessPiece p a, [ChessPiece p a]) m
passFriendly = gate $ (\(((p, t), x), xs) -> null [x |((p', t'), x') <- xs, p' == p, x' == x])
passCapture :: Monoid m => Rule (ChessPiece p a, [ChessPiece p a]) m
passCapture = pass
passNoCapture :: Monoid m => Rule (ChessPiece p a, [ChessPiece p a]) m
passNoCapture = pass
passCheck :: Monoid m => Rule (ChessState p a) m
passCheck = pass

kleeneStar r = (passBoard /. passFriendly /. passNoCapture /. ((pass /* r) /* pass) /. passBoard) //: 8

type IntPiece = ChessPiece (Vec Int) (Vec Int)
type IntPieceMove = ((Vec Int, ChessPieceType),  (Vec Int, Vec Int))

rookMove :: Rule (IntPiece, [IntPiece]) (IntPieceMove, Select IntPieceMove)
rookMove = kleeneStar sqU // kleeneStar sqD // kleeneStar sqR // kleeneStar sqL

