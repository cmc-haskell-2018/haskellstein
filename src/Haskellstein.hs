module Haskellstein where

import System.Environment
import Foreign.C.Types
import Foreign.C.String
import Haskellstein.Initialize
import Haskellstein.Raycasting
import Haskellstein.Data
import Haskellstein.Interactions

--Initing GUI workspace
foreign import ccall "_Z14init_workspaceiiPciiS_S_S_" cInitWorkspace ::
  CInt --Window width
  -> CInt --Window height
  -> CString --Window title
  -> CInt --Window frame rate limit
  -> CInt --Scale factor
  -> CString --Wall texture path
  -> CString --Enemy texture path
  -> CString --Sprite texture path
  -> IO()

--Updating GUI workspace
foreign import ccall "_Z16update_workspacev" cUpdateWorkspace :: IO()

--Get delta time
foreign import ccall "_Z14get_delta_timev" cGetDeltaTime :: IO(CFloat)

--Key constans
cKeyW :: CInt
cKeyW = 0

cKeyA :: CInt
cKeyA = 1

cKeyS :: CInt
cKeyS = 2

cKeyD :: CInt
cKeyD = 3

cKeyI :: CInt
cKeyI = 4

cKeySpace :: CInt
cKeySpace = 5

--Get key pressed
foreign import ccall "_Z15get_key_pressedi" cGetKeyPressed :: CInt -> IO(Int)

greatCycle :: Scene -> IO()
greatCycle scene = do
  drawScene scene
  cUpdateWorkspace
  deltaTime     <- cGetDeltaTime
  keyWState     <- cGetKeyPressed cKeyW
  keySState     <- cGetKeyPressed cKeyS
  keyDState     <- cGetKeyPressed cKeyD
  keyAState     <- cGetKeyPressed cKeyA
  keySpaceState <- cGetKeyPressed cKeySpace
  let tmpDelta  = realToFrac deltaTime
  let delta     = if (tmpDelta > 0.1) then 0.1 else tmpDelta
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
      then putStrLn "GameOver"
      else greatCycle . doInteractions $ newScene

start :: IO()
start = do
    args <- getArgs
    if null args then do
        putStrLn "No file path"
    else do
        let
          windowWidth          = 800
          windowHeight         = 600
          windowName           = "Haskellstein"
          windowFramerateLimit = 100
          scaleFactor          = 1
          wallTextureName      = "data/textures/wall.png"
          enemyTextureName     = "data/textures/enemy.png"
          spriteTextureName    = "data/textures/sprite.png"
        tilemap                <- readFile . head $ args
        windowTitle            <- newCString windowName
        wallTexturePath        <- newCString wallTextureName
        enemyTexturePath       <- newCString enemyTextureName
        spriteTexturePath      <- newCString spriteTextureName
        cInitWorkspace windowWidth
                       windowHeight
                       windowTitle
                       windowFramerateLimit
                       scaleFactor
                       wallTexturePath
                       enemyTexturePath
                       spriteTexturePath
        greatCycle . createScene . createTilemap $ tilemap
