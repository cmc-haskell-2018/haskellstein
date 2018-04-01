module Haskellstein.Engine.Raycasting where

import Data.Maybe

import Haskellstein.Engine.Camera
import Haskellstein.Engine.TileMap
import Haskellstein.Engine.Vector

-- * Raycasting

-- | Raycast using a 'Camera' and a map.
raycastWithTileMap
  :: Int        -- ^ Maximum ray path length (in tile).
  -> Int        -- ^ Number of rays.
  -> TileMap a  -- ^ A tile map.
  -> Camera     -- ^ A camera.
  -> [[Hit a]]  -- ^ A list of hits for every ray.
raycastWithTileMap n w tm = map (castRayWithTileMap n tm) . cameraRays w

-- | Cast a single ray and collect all objects on its path.
castRayWithTileMap
  :: Int        -- ^ Maximum ray path length (in tiles).
  -> TileMap a  -- ^ A tile map.
  -> Ray        -- ^ Ray.
  -> [Hit a]    -- ^ A list of objects the ray hits on its path.
castRayWithTileMap n tm ray = mapMaybe f (take n (rayPath ray))
  where
    f info = Hit info <$> tileAt (hitCoords info) tm

-- | Compute ray's path through a discrete 2D space.
--
-- NOTE: the path does not contain the starting tile.
rayPath :: Ray -> [HitInfo]
rayPath ray@(Ray point (rx, ry)) = go (dx * sideDistX, dy * sideDistY) startingCell
  where
    startingCell = pointToCoords point
    (sideDistX, sideDistY) = raySideDist ray

    -- coefficients for X and Y axis movements
    (dx, dy) = (abs ry, abs rx)

    -- directions for X and Y axis
    di = floor (signum rx)
    dj = floor (signum ry)

    -- infinite raycasting interation
    go (sx, sy) (i, j)
      | sx <= sy  = mkHitInfo ray SideX (i + di, j) : go (sx + dx, sy) (i + di, j)
      | otherwise = mkHitInfo ray SideY (i, j + dj) : go (sx, sy + dy) (i, j + dj)

-- ** Hit info

-- | Raycasting hit.
data HitInfo = HitInfo
  { hitSide     :: Side    -- ^ Which side was hit by the ray.
  , hitCoords   :: Coords  -- ^ Coordinates of the hit tile.
  , hitDistance :: Float   -- ^ Distance from ray origin to hit.
  , hitPosition :: Float   -- ^ Where exactly the wall was hit.
  } deriving (Eq, Show)

-- | An object that's been hit by a ray.
data Hit a = Hit
  { hitInfo   :: HitInfo  -- ^ Raycasting hit info.
  , hitObject :: a        -- ^ An object.
  } deriving (Eq, Show)

-- | Side of a tile.
data Side
  = SideX  -- ^ A side that can be hit from the X axis.
  | SideY  -- ^ A side that can be hit from the Y axis.
  deriving (Eq, Show)

-- | Compute hit info given 'Ray', hit 'Side' and hit tile coordinates.
mkHitInfo :: Ray -> Side -> Coords -> HitInfo
mkHitInfo ray side coords = HitInfo side coords distance pos
  where
    distance = rayCellHitDistance ray side coords
    pos = rayHitPosition ray side distance

-- | Compute a distance to a hit tile.
--
-- >>> rayCellHitDistance (Ray (0.4, 0.6) (1.23, -0.45)) SideY (7,-3)
-- 7.567345
rayCellHitDistance :: Ray -> Side -> Coords -> Float
rayCellHitDistance (Ray (x, y) (rx, ry)) side (i, j) =
  case side of
    SideX -> (fromIntegral i - x + (1 - signum rx) / 2) / rx
    SideY -> (fromIntegral j - y + (1 - signum ry) / 2) / ry

rayHitPosition :: Ray -> Side -> Float -> Float
rayHitPosition (Ray (x, y) (rx, ry)) side distance =
  case side of
    SideX -> fractionOf (y + distance * ry)
    SideY -> fractionOf (x + distance * rx)

-- * Rays

-- | Ray direction.
data Ray = Ray
  { rayOrigin    :: Point   -- ^ Ray starting point.
  , rayDirection :: Vector  -- ^ Ray direction. Can be non-normalised.
  } deriving (Eq, Show)

-- | Produce a specified number of rays for a camera.
--
-- >>> cameraRays 5 (Camera (0, 0) (1, 0) (0, 1))
-- [(1.0,-1.0),(1.0,-0.5),(1.0,0.0),(1.0,0.5),(1.0,1.0)]
cameraRays :: Int -> Camera -> [Ray]
cameraRays n camera
  | n < 0  = []
  | n == 1 = [Ray (cameraPosition camera) (cameraDirection camera)]
  | otherwise  = map makeRay [0 .. n - 1]
  where
    makeRay i = Ray (cameraPosition camera) (dir .+ (d .* plane))
      where
        d = 2 * fromIntegral i / (fromIntegral n - 1) - 1
        dir = cameraDirection camera
        plane = cameraPlane camera

-- * Helpers

-- | Extract the fraction part of a number.
--
-- >>> fractionOf 1.2 :: Float
-- 0.20000005
--
-- >>> fractionOf (-1.2) :: Float
-- 0.79999995
fractionOf :: RealFrac a => a -> a
fractionOf x = x - fromIntegral (floor x :: Int)

-- | Distances to the next tile boundaries along each coordinate.
--
-- >>> raySideDist (0.3, 0.6) (1, -1)
-- (0.7,0.6)
raySideDist :: Ray -> (Float, Float)
raySideDist (Ray (x, y) (rx, ry)) = (sideDistX, sideDistY)
  where
    fx = fractionOf x
    fy = fractionOf y

    sideDistX
      | rx < 0    = fx
      | otherwise = 1 - fx

    sideDistY
      | ry < 0    = fy
      | otherwise = 1 - fy

