module Haskellstein where

import SFML.Graphics (Texture, textureFromFile, setSmooth)
import SFML.Window.Event
import SFML.Window.Keyboard

import Haskellstein.Engine
import Haskellstein.Player

-- | Handle window events.
handleEvent :: SFEvent -> Player -> Player
-- set rotation and movement with WASD keys
handleEvent (SFEvtKeyPressed KeyW _ _ _ _) = setPlayerSpeed 2
handleEvent (SFEvtKeyPressed KeyA _ _ _ _) = setPlayerRotation (-2)
handleEvent (SFEvtKeyPressed KeyS _ _ _ _) = setPlayerSpeed (-2)
handleEvent (SFEvtKeyPressed KeyD _ _ _ _) = setPlayerRotation 2
-- reset rotation and movement when keys are released
handleEvent (SFEvtKeyReleased KeyW _ _ _ _) = setPlayerSpeed 0
handleEvent (SFEvtKeyReleased KeyA _ _ _ _) = setPlayerRotation 0
handleEvent (SFEvtKeyReleased KeyS _ _ _ _) = setPlayerSpeed 0
handleEvent (SFEvtKeyReleased KeyD _ _ _ _) = setPlayerRotation 0
-- ignore all other events
handleEvent _ = id

-- | A sample map with 'Char' objects in map cells.
sampleMap :: TileMap Char
sampleMap = asciiTileMap
  [ "#@#@#@#@#@#@#@#@#@"
  , "#                #"
  , "#@#@#@#          #"
  , "#   #     #      #"
  , "#   #     #      #"
  , "#   #@#@#@# #@#@#@"
  , "#      #         #"
  , "#           #    #"
  , "#@#@#@#@#@#@#@#@#@"
  ]

-- | Make a textured map from an ASCII map.
makeWorldMap :: Texture -> TileMap Char -> TileMap Picture
makeWorldMap wallTexture = fmap f
  where
    f '#' = PictureTexture wallTexture 0  (64, 64)
    f _   = PictureTexture wallTexture 64 (64, 64)

run :: IO ()
run = do
  Right wallTexture <- textureFromFile "textures/wall.png" Nothing
  setSmooth wallTexture True
  let tileMap = makeWorldMap wallTexture sampleMap
  play engineSettings initPlayer (const tileMap) playerCamera handleEvent (updatePlayer tileMap)
  where
    engineSettings = defaultEngineSettings
      { engineWindowTitle = "Haskellstein"
      , engineWindowSize  = (640, 480)
      }
