module Haskellstein.Player where

import Data.Function ((&))
import Haskellstein.Engine.Camera
import Haskellstein.Engine.TileMap
import Haskellstein.Engine.Vector

-- | Player state.
data Player = Player
  { playerCamera    :: Camera   -- ^ Player camera location and direction.
  , playerRotation  :: Float    -- ^ Player rotation speed.
  , playerSpeed     :: Float    -- ^ Player movement speed.
  }

-- | Initialise 'Player'.
initPlayer :: Player
initPlayer = Player
  { playerCamera = Camera (15, 1.5) dir plane
  , playerRotation = 0
  , playerSpeed    = 0
  }
  where
    theta = -0.3
    dir = rotate theta (-1, 0)
    plane = rotate (theta + pi/2) (-0.66, 0)

-- | Set 'Player' rotation speed.
setPlayerRotation :: Float -> Player -> Player
setPlayerRotation theta player = player { playerRotation = theta }

-- | Set 'Player' movement speed.
setPlayerSpeed :: Float -> Player -> Player
setPlayerSpeed speed player = player { playerSpeed = speed }

-- | Update 'Player': move and rotate.
updatePlayer :: TileMap a -> Float -> Player -> Player
updatePlayer tileMap dt player = player { playerCamera = newCamera }
  where
    newCamera = playerCamera player
      & safeMoveCamera 0.1 tileMap (dt * playerSpeed player)
      & rotateCamera (dt * playerRotation player)

