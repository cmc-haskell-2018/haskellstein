module Haskellstein.Engine.Camera where

import Haskellstein.Engine.Vector
import Haskellstein.Engine.TileMap

-- | A camera for raycasting.
data Camera = Camera
  { cameraPosition    :: Vector   -- ^ Camera position.
  , cameraDirection   :: Vector   -- ^ Camera direction.
  , cameraPlane       :: Vector
    -- ^ Camera plane vector (always orthogonal to camera direction).
  } deriving (Show)

-- | Rotate camera by a given angle (in radians).
rotateCamera :: Float -> Camera -> Camera
rotateCamera theta camera = camera
  { cameraDirection = rotate theta (cameraDirection camera)
  , cameraPlane     = rotate theta (cameraPlane camera)
  }

-- | Move camera by a given distance (in camera direction).
moveCamera :: Float -> Camera -> Camera
moveCamera d camera = camera
  { cameraPosition = cameraPosition camera .+ (d .* cameraDirection camera)
  }

-- | Move camera without going through walls.
safeMoveCamera
  :: Float      -- ^ Minimal distance to a wall.
  -> TileMap a  -- ^ A tile map of walls.
  -> Float      -- ^ Distance to travel.
  -> Camera     -- ^ Current camera.
  -> Camera
safeMoveCamera p tileMap d camera = camera { cameraPosition = newPosition }
 where
    (x, y) = cameraPosition camera
    (i, j) = pointToCoords (cameraPosition camera)

    (x', y') = cameraPosition (moveCamera d camera)
    (i', j') = pointToCoords (x + p * signum dx, y + p * signum dy)
    (dx, dy) = cameraDirection camera

    isSafe Nothing = True   -- no wall is a safe
    isSafe _       = False  -- any wall is unsafe to go through

    safeX  = isSafe (tileAt (i', j ) tileMap)
    safeY  = isSafe (tileAt (i,  j') tileMap)
    safeXY = isSafe (tileAt (i', j') tileMap)

    newPosition
      -- move diagonally only if we don't have to cut corners
      | safeXY && safeX && safeY = (x', y')
      | safeX     = (x', y)
      | safeY     = (x, y')
      | otherwise = (x, y)
