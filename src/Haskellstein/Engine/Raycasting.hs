{-# OPTIONS_GHC -fno-warn-type-defaults #-}
module Haskellstein.Engine.Raycasting where

type Vector = (Float, Float)

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

-- | A camera for raycasting.
data Camera = Camera
  { cameraPosition    :: Vector   -- ^ Camera position.
  , cameraDirection   :: Vector   -- ^ Camera direction.
  , cameraPlane       :: Vector
    -- ^ Camera plane vector (always orthogonal to camera direction).
  } deriving (Show)

-- | Ray direction.
type Ray = Vector

-- | Produce a specified number of rays for a camera.
--
-- >>> cameraRays 5 (Camera (0, 0) (1, 0) (0, 1))
-- [(1.0,-1.0),(1.0,-0.5),(1.0,0.0),(1.0,0.5),(1.0,1.0)]
cameraRays :: Int -> Camera -> [Ray]
cameraRays n camera
  | n < 0  = []
  | n == 1 = [cameraDirection camera]
  | otherwise  = map makeRay [0 .. n - 1]
  where
    makeRay i = dir .+ (d .* plane)
      where
        d = 2 * fromIntegral i / (fromIntegral n - 1) - 1
        dir = cameraDirection camera
        plane = cameraPlane camera

-- | Map cell coordinates.
type MapCoords = (Int, Int)

-- | Find map cell coords corresponding to a given point.
--
-- >>> pointToMapCoords (23.4, -19.7)
-- (23,-20)
pointToMapCoords :: Point -> MapCoords
pointToMapCoords (x, y) = (floor x, floor y)

-- | Distances to the next cell boundaries along each coordinate.
--
-- >>> raySideDist (0.3, 0.6) (1, -1)
-- (0.7,0.6)
raySideDist :: Point -> Ray -> (Float, Float)
raySideDist (x, y) (rx, ry) = (sideDistX, sideDistY)
  where
    fx = x - fromIntegral (floor x)
    fy = y - fromIntegral (floor y)

    sideDistX
      | rx < 0    = fx
      | otherwise = 1 - fx

    sideDistY
      | ry < 0    = fy
      | otherwise = 1 - fy

-- | Compute ray's path through a discrete 2D space.
--
-- >>> take 10 $ rayPathFrom (0.3, 0.8) (1.2, 3.4)
-- [(0,0),(0,1),(0,2),(1,2),(1,3),(1,4),(1,5),(2,5),(2,6),(2,7)]
--
-- >>> take 10 $ rayPathFrom (0.3, 0.8) (1.2, -3.4)
-- [(0,0),(0,-1),(0,-2),(1,-2),(1,-3),(1,-4),(1,-5),(2,-5),(2,-6),(2,-7)]
--
-- >>> take 10 $ rayPathFrom (0.4, -0.8) (1, 0)
-- [(0,-1),(1,-1),(2,-1),(3,-1),(4,-1),(5,-1),(6,-1),(7,-1),(8,-1),(9,-1)]
rayPathFrom :: Point -> Ray -> [MapCoords]
rayPathFrom point ray@(rx, ry) = go (dx * sideDistX, dy * sideDistY) startingCell
  where
    startingCell = pointToMapCoords point
    (sideDistX, sideDistY) = raySideDist point ray

    -- coefficients for X and Y axis movements
    (dx, dy) = (abs ry, abs rx)

    -- directions for X and Y axis
    di = floor (signum rx)
    dj = floor (signum ry)

    -- infinite raycasting interation
    go (sx, sy) (i, j) = (i, j) : coords
      where
        coords
          | sx <= sy  = go (sx + dx, sy) (i + di, j)
          | otherwise = go (sx, sy + dy) (i, j + dj)

