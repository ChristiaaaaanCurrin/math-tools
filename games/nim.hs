
{-# LANGUAGE MultiParamTypeClasses #-}

module Nim where

import Data.Semigroup
import Move
import Group
import Rule

type Vec a = Sum a
vec x = Sum x
:x

instance Enum a => Enum (Sum a) where
  toEnum x = vec (toEnum x)
  fromEnum = fromEnum.num

instance (Num a) => Group (Sum a) where
  inv = negate

instance (Num a) => Move (Sum a) (Sum a) where
  (#) = (<>)

mport Vec

nim :: Rule (Vec Int) (Vec Int)
nim n = [-n.. -1]
