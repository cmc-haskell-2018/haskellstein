module Haskellstein where

import SFML.Graphics (Color(..), textureFromFile)
import SFML.Window.Event
import SFML.Window.Keyboard

import Haskellstein.Engine
import Haskellstein.Engine.Raycasting

run :: IO ()
run = do
  Right wallTexture <- textureFromFile "textures/wall.png" Nothing
  play title winSize bgColor raycasterDepth initWorld (worldMap wallTexture) worldCamera handleEvent
  where
    title = "Haskellstein"
    winSize = (640, 480)
    bgColor = Color 32 32 32 255
    raycasterDepth = 100

    initWorld = Camera (16, 1.5) dir plane
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

