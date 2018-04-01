module Haskellstein.Engine.Vector where

-- | Vector in a 2D plane.
type Vector = (Float, Float)

-- | A point on a 2D plane.
type Point = (Float, Float)

-- | Vector addition.
(.+) :: Vector -> Vector -> Vector
(x, y) .+ (u, v) = (x + u, y + v)

-- | Vector subtraction.
(.-) :: Vector -> Vector -> Vector
(x, y) .- (u, v) = (x - u, y - v)

-- | Scalar by vector multiplication.
(.*) :: Float -> Vector -> Vector
a .* (x, y) = (a * x, a * y)

-- | Rotate a vector by a given angle (in radians).
rotate :: Float -> Vector -> Vector
rotate theta (x, y) = (x', y')
  where
    x' = x * cos theta - y * sin theta
    y' = x * sin theta + y * cos theta

