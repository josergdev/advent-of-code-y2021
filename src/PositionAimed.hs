module PositionAimed where

import qualified Position as Pos

type Aim = Integer

data PositionAimed = PositionAimed Pos.Position Aim deriving (Show)

initial :: PositionAimed
initial = PositionAimed Pos.initial 0

down :: PositionAimed -> Integer -> PositionAimed
down (PositionAimed p a) x = PositionAimed p (a + x)

up :: PositionAimed -> Integer -> PositionAimed
up (PositionAimed p a) x = PositionAimed p (a - x)

forward :: PositionAimed -> Integer -> PositionAimed
forward (PositionAimed (Pos.Position h d) a) x = PositionAimed (Pos.Position (h + x) (d + (a * x))) a

product :: PositionAimed -> Integer
product (PositionAimed p a) = Pos.product p