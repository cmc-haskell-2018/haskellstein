module Main where

import Foreign.C.Types
import Foreign.C.String
import Haskellstein.Initialize
import Haskellstein.Raycasting
import Haskellstein.Data
import Haskellstein.Interactions
import System.Environment

--Initing GUI workspace
foreign import ccall "_Z14init_workspaceiiPciS_S_S_" cInitWorkspace ::
  CInt --Window width
  -> CInt --Window height
  -> CString --Window title
  -> CInt --Window frame rate limit
  -> CString --Wall texture path
  -> CString --Enemy texture path
  -> CString --Sprite texture path
  -> IO()

--Updating GUI workspace
foreign import ccall "_Z16update_workspacev" cUpdateWorkspace :: IO()

greatCycle :: Scene -> IO()
greatCycle scene = do

  drawScene scene
  cUpdateWorkspace
  greatCycle . doInteractions $ scene

main :: IO()
main = do
    args <- getArgs
    if null args then do
        putStrLn "No file path"
    else do
        let
          windowWidth = 800
          windowHeight = 600
          windowName = "Haskellstein"
          windowFramerateLimit = 100
          wallTextureName = "data/textures/wall.png"
          enemyTextureName = "data/textures/enemy.png"
          spriteTextureName = "data/textures/sprite.png"

        tilemap <- readFile . head $ args
        windowTitle <- newCString windowName
        wallTexturePath <- newCString wallTextureName
        enemyTexturePath <- newCString enemyTextureName
        spriteTexturePath <- newCString spriteTextureName
        cInitWorkspace windowWidth windowHeight windowTitle windowFramerateLimit
          wallTexturePath enemyTexturePath spriteTexturePath
        greatCycle . createScene . createTilemap $ tilemap
