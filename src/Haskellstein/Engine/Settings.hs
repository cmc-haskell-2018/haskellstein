module Haskellstein.Engine.Settings where

import SFML.Graphics (Color(..))

-- | Haskellstein raycasting engine settings.
data EngineSettings = EngineSettings
  { engineWindowTitle     :: String       -- ^ Window title.
  , engineWindowSize      :: (Int, Int)   -- ^ Window size.
  , engineCeilingColor    :: Color        -- ^ Ceiling color.
  , engineFloorColor      :: Color        -- ^ Floor color.
  , engineFramerateLimit  :: Maybe Int    -- ^ Optional framerate limit.
  , engineDisplayFPS      :: Bool         -- ^ Display current framerate?
  , engineRaycasterDepth  :: Int          -- ^ Raycaster depth (in cells).
  }

-- | Default engine settings.
defaultEngineSettings :: EngineSettings
defaultEngineSettings = EngineSettings
  { engineWindowTitle     = "Default title"
  , engineWindowSize      = (640, 480)
  , engineCeilingColor    = Color 32 32 32 255
  , engineFloorColor      = Color 64 64 64 255
  , engineFramerateLimit  = Just 60
  , engineDisplayFPS      = False
  , engineRaycasterDepth  = 1000
  }
