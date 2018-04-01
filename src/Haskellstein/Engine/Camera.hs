module Haskellstein.Engine.Camera where

import Haskellstein.Engine.Vector

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


