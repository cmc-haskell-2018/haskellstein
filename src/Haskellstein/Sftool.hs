module Haskellstein.Sftool where

import Foreign.C.Types
import Foreign.C.String

type WindowStats = (Int, Int, String, Int, Int)
type TexturePaths = (String, String, String)
type Line = (Int, Int, Double, Int, Int)

----Shell functions

initWorkspace :: WindowStats -> TexturePaths -> IO()
initWorkspace
  (windowWidth, windowHeight, windowTitle, windowFrameLimit, windowScale)
  (wallTexturePath, enemyTexturePath, spriteTexturePath) = do
  cWindowTitle       <- newCString windowTitle
  cWallTexturePath   <- newCString wallTexturePath
  cEnemyTexturePath  <- newCString enemyTexturePath
  cSpriteTexturePath <- newCString spriteTexturePath
  cInitWorkspace
    (fromIntegral windowWidth)
    (fromIntegral windowHeight)
    cWindowTitle
    (fromIntegral windowFrameLimit)
    (fromIntegral windowScale)
    cWallTexturePath
    cEnemyTexturePath
    cSpriteTexturePath

updateWorkspace :: IO()
updateWorkspace = do
  cUpdateWorkspace

getDeltaTime :: IO(Float)
getDeltaTime = do
  deltaTime <- cGetDeltaTime
  return $ realToFrac deltaTime

getKeyPressed :: Int -> IO(CInt)
getKeyPressed chosenKey = do
  keyState <- cGetKeyPressed $ fromIntegral chosenKey
  return $ fromIntegral keyState

pushDrawBuffer :: Int -> Int -> IO()
pushDrawBuffer linesCount textureType = do
  cPushDrawBuffer (fromIntegral linesCount) (fromIntegral textureType)

drawLine :: Line -> IO()
drawLine (chosenLine, xCoord, height, texCoord, color) = do
  cDrawLine
    (fromIntegral chosenLine)
    (fromIntegral xCoord)
    (realToFrac height)
    (fromIntegral texCoord)
    (fromIntegral color)

getLinesCount :: Int
getLinesCount = fromIntegral cGetLinesCount

getWindowHeight :: Int
getWindowHeight = fromIntegral cGetWindowHeight

getScaleFactor :: Int
getScaleFactor = fromIntegral cGetScaleFactor

setHealthBarSize :: Int -> IO()
setHealthBarSize size = do
  cSetHealthBarSize $ fromIntegral size

----Key constans

keyW :: Int
keyW = 0

keyA :: Int
keyA = 1

keyS :: Int
keyS = 2

keyD :: Int
keyD = 3

keyI :: Int
keyI = 4

keySpace :: Int
keySpace = 5

----FFI calls

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

--Get key pressed
foreign import ccall "_Z15get_key_pressedi" cGetKeyPressed :: CInt -> IO(Int)

--Push chosen part of draw buffer
foreign import ccall "_Z16push_draw_bufferii" cPushDrawBuffer ::
  CInt --Lines count
  -> CInt --Texture type
  -> IO()

--Draw one line
foreign import ccall "_Z9draw_lineiidii" cDrawLine ::
  CInt --Chosen line
  -> CInt --X coord
  -> CDouble --Height
  -> CInt --Texture
  -> CInt --Color
  -> IO()

--Get screen's line's count
foreign import ccall "_Z15get_lines_countv" cGetLinesCount :: CInt

--Get window's height
foreign import ccall "_Z17get_window_heightv" cGetWindowHeight :: CInt

--Get window's scale factor
foreign import ccall "_Z16get_scale_factorv" cGetScaleFactor :: CInt

--Set health bar size
foreign import ccall "_Z19set_health_bar_sizei" cSetHealthBarSize ::
  CInt --Size
  -> IO()
