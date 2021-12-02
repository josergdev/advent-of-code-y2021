module Position where

type Horizontal = Integer

type Depth = Integer 

data Position = Position Horizontal Depth deriving (Show)

initial :: Position
initial = Position 0 0

down :: Position -> Integer -> Position
down (Position h d) x = Position h (d + x)

up :: Position -> Integer -> Position
up (Position h d) x = Position h (d - x)

forward :: Position -> Integer -> Position
forward (Position h d) x = Position (h + x) d

product :: Position -> Integer
product (Position h d) = h * d
