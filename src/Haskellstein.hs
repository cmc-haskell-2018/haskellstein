module Haskellstein where

import System.Environment
import Haskellstein.Sftool
import Haskellstein.Raycaster
import Haskellstein.Initialize
import Haskellstein.Data
import Haskellstein.Interactions

greatCycle :: Scene -> IO()
greatCycle scene = do
  setHealthBarSize $ pHp $ sPlayer scene
  drawScene scene
  cUpdateWorkspace
  deltaTime     <- getDeltaTime
  keyWState     <- getKeyPressed keyW
  keySState     <- getKeyPressed keyS
  keyDState     <- getKeyPressed keyD
  keyAState     <- getKeyPressed keyA
  keySpaceState <- getKeyPressed keySpace
  let delta     = if (deltaTime > 0.1) then 0.1 else deltaTime
--update scene condition
  let newScene  =
        Scene
          (sPlayer scene)
          (sFireball scene)
          (sEnemy scene)
          (sTilemap scene)
          (Control
              (keyWState /= 0)
              (keyAState /= 0)
              (keySState /= 0)
              (keyDState /= 0)
              (keySpaceState /= 0))
          delta
  if (((pHp . sPlayer $ scene) <= 0) || (null . sEnemy $ scene))
      then
        putStrLn "GameOver"
      else
        greatCycle . doInteractions $ newScene

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
        greatCycle . createScene . createTilemap $ tilemap
