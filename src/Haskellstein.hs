module Haskellstein where

import System.Environment
import Haskellstein.Sftool
import Haskellstein.Raycaster
import Haskellstein.Initialize
import Haskellstein.Data
import Haskellstein.Picture
import Haskellstein.Interactions

start :: Int -> IO()
start turn = do
    allargs <- getArgs
    if length allargs <= turn then do
        putStrLn "Victory"
    else do
        let
          windowWidth       = 800
          windowHeight      = 600
          windowTitle       = "Haskellstein"
          windowFrameLimit  = 100
          scaleFactor       = 1
          wallTexturePath   = "data/textures/wall.png"
          enemyTexturePath  = "data/textures/enemy1.png"
          spriteTexturePath = "data/textures/sprite1.png"
          myarg             = allargs !! turn
        tilemap             <- readFile myarg
        initWorkspace
          (windowWidth, windowHeight, windowTitle,
            windowFrameLimit, scaleFactor)
          (wallTexturePath, enemyTexturePath, spriteTexturePath)
        levelEnd <- greatCycle (createScene . createTilemap $ tilemap)
                               stepScene
                               updateScene
                               endCheck
                               makePicture
        if levelEnd then start $ turn + 1
        else putStrLn "Defeat"

--GameLoop
greatCycle
  :: a --object
  -> (a -> a) --stepObject
  -> (a -> Control -> Float -> a) --getControlAndDelta
  -> (a -> GameEnd) --checkEndCondition
  -> (a -> Picture) --drawObject
  -> IO(Bool)
greatCycle scene step update end picture =
  if ((end scene) == Victory) then return True
  else if ((end scene) == Defeat) then return False
  else do
      displayPicture $ picture $ scene
      control      <- getControl
      delta        <- getDelta
      let newScene = step $ update scene control delta
      greatCycle newScene step update end picture

--visualizeScene
displayPicture :: Picture -> IO()
displayPicture picture = do
  setHealthBarSize (piHp picture)
  drawScene picture
  updateWorkspace

--read pushed keys
getControl :: IO(Control)
getControl = do
  keyWState      <- getKeyPressed keyW
  keySState      <- getKeyPressed keyS
  keyAState      <- getKeyPressed keyA
  keyDState      <- getKeyPressed keyD
  keyQState      <- getKeyPressed keyLeftArrow
  keyEState      <- getKeyPressed keyRightArrow
  keySpaceState  <- getKeyPressed keySpace
  keyTurn        <- getKeyPressed keyI
  let newControl = Control
                       (keyWState /= 0)
                       (keySState /= 0)
                       (keyAState /= 0)
                       (keyDState /= 0)
                       (keyQState /= 0)
                       (keyEState /= 0)
                       (keySpaceState /= 0)
                       (keyTurn /= 0)
  return newControl

--get delta time
getDelta :: IO(Float)
getDelta = do
  deltaTime <- getDeltaTime
  let delta = if (deltaTime > 0.1) then 0.1 else deltaTime
  return delta

--addExternalData
updateScene :: Scene -> Control -> Float -> Scene
updateScene scene control delta = scene {sControl = control, sDelta = delta}

--makeInteractions
endCheck :: Scene -> GameEnd
endCheck scene =
  if ((pHp . sPlayer $ scene) <= 0) then Defeat
  else if (pExit . sPlayer $ scene) then Victory
       else Continue

--makeInteractions
stepScene :: Scene -> Scene
stepScene scene = doInteractions scene
