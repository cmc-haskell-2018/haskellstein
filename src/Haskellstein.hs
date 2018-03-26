module Haskellstein where

import System.Environment
import Haskellstein.Sftool
import Haskellstein.Raycaster
import Haskellstein.Initialize
import Haskellstein.Data
import Haskellstein.Interactions

start :: IO()
start = do
    args <- getArgs
    if null args then do
        putStrLn "No file path"
    else do
        let
          windowWidth       = 800
          windowHeight      = 600
          windowTitle       = "Haskellstein"
          windowFrameLimit  = 100
          scaleFactor       = 1
          wallTexturePath   = "data/textures/wall.png"
          enemyTexturePath  = "data/textures/enemy.png"
          spriteTexturePath = "data/textures/sprite.png"
        tilemap             <- readFile . head $ args
        initWorkspace
          (windowWidth, windowHeight, windowTitle,
            windowFrameLimit, scaleFactor)
          (wallTexturePath, enemyTexturePath, spriteTexturePath)
        greatCycle (Just (createScene . createTilemap $ tilemap))
                   stepScene
                   updateScene
                   displayScene

--GameLoop
greatCycle
  :: Maybe a --object
  -> (a -> Maybe a) --stepObject
  -> (a -> IO(a)) --updateObject
  -> (a -> IO()) --drawObject
  -> IO()
greatCycle Nothing _ _ _                 = putStrLn "GameOver"
greatCycle (Just scene) step update draw = do
  draw scene
  updatedScene <- update scene
  greatCycle (step updatedScene) step update draw

--visualizeScene
displayScene :: Scene -> IO()
displayScene scene = do
  setHealthBarSize $ pHp $ sPlayer scene
  drawScene scene
  updateWorkspace

--includeInput
updateScene :: Scene -> IO(Scene)
updateScene scene = do
  deltaTime     <- getDeltaTime
  keyWState     <- getKeyPressed keyW
  keyAState     <- getKeyPressed keyA
  keySState     <- getKeyPressed keyS
  keyDState     <- getKeyPressed keyD
  keyQState     <- getKeyPressed keyQ
  keyEState     <- getKeyPressed keyE
  keySpaceState <- getKeyPressed keySpace
  let delta     = if (deltaTime > 0.1) then 0.1 else deltaTime
  let newScene  = Scene
                      (sPlayer scene)
                      (sFireball scene)
                      (sEnemy scene)
                      (sTilemap scene)
                      (Control
                          (keyWState /= 0)
                          (keyAState /= 0)
                          (keySState /= 0)
                          (keyDState /= 0)
                          (keyQState /= 0)
                          (keyEState /= 0)
                          (keySpaceState /= 0))
                      delta
  return newScene

--makeInteractions
stepScene :: Scene -> Maybe Scene
stepScene scene =
  if (((pHp . sPlayer $ scene) <= 0) || (null . sEnemy $ scene))
      then
        Nothing
      else
        Just (doInteractions scene)
