module Haskellstein where

import SFML.Graphics (textureFromFile)
import SFML.Window.Event
import SFML.Window.Keyboard

import Haskellstein.Engine
import Haskellstein.Engine.Raycasting

run :: IO ()
run = do
  Right wallTexture <- textureFromFile "textures/wall.png" Nothing
  play engineSettings initWorld (worldMap wallTexture) worldCamera handleEvent updateWorld
  where
    engineSettings = defaultEngineSettings
      { engineWindowTitle = "Haskellstein"
      , engineWindowSize  = (640, 480)
      }

    initWorld = Camera (15, 1.5) dir plane
      where
        theta = -0.3
        dir = rotate theta (-1, 0)
        plane = rotate (theta + pi/2) (-0.66, 0)

    worldMap wallTexture _ = sampleMap wallTexture
    worldCamera = id

    handleEvent (SFEvtKeyPressed KeyW _ _ _ _) = moveCamera 0.1
    handleEvent (SFEvtKeyPressed KeyA _ _ _ _) = rotateCamera (-0.1)
    handleEvent (SFEvtKeyPressed KeyS _ _ _ _) = moveCamera (-0.1)
    handleEvent (SFEvtKeyPressed KeyD _ _ _ _) = rotateCamera 0.1
    handleEvent _ = id

    updateWorld dt = moveCamera dt . rotateCamera (- dt)

    sampleMap wallTexture (i, j) = (xs !! j) !! i
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
    
        f '#' = Just (PictureTexture wallTexture 0  (64, 64))
        f '@' = Just (PictureTexture wallTexture 64 (64, 64))
        f _ = Nothing

