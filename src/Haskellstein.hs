module Haskellstein where

import SFML.Graphics (Texture, textureFromFile, setSmooth)
import SFML.Window.Event
import SFML.Window.Keyboard

import Haskellstein.Engine
import Haskellstein.Engine.Raycasting

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
sampleMap :: MapCoords -> Maybe Char
sampleMap (i, j) = (xs !! j) !! i
  where
    xs = map (map f)
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

    f ' ' = Nothing
    f c   = Just c

-- | Make a textured map from an ASCII map.
makeWorldMap :: Texture -> (MapCoords -> Maybe Char) -> (MapCoords -> Maybe Picture)
makeWorldMap wallTexture m = fmap f . m
  where
    f '#' = PictureTexture wallTexture 0  (64, 64)
    f _   = PictureTexture wallTexture 64 (64, 64)

run :: IO ()
run = do
  Right wallTexture <- textureFromFile "textures/wall.png" Nothing
  setSmooth wallTexture True
  play engineSettings initPlayer (worldMap wallTexture) playerCamera handleEvent updatePlayer
  where
    engineSettings = defaultEngineSettings
      { engineWindowTitle = "Haskellstein"
      , engineWindowSize  = (640, 480)
      , engineFramerateLimit = Nothing
      }

    worldMap wallTexture _ = makeWorldMap wallTexture sampleMap

