module Haskellstein.Sftool where

import Foreign.C.Types
import Foreign.C.String

type WindowStats = (Int, Int, String, Int, Int)
type TexturePaths = (String, String, String)
type ExtraPaths = (String, String, String)
type Line = (Int, Int, Double, Int, Int)
type Text = (String, Int, Int, Int)

----Shell functions

initWorkspace :: WindowStats -> TexturePaths -> ExtraPaths -> IO()
initWorkspace
  (windowWidth, windowHeight, windowTitle, windowFrameLimit, windowScale)
  (wallTexturePath, enemyTexturePath, spriteTexturePath)
  (shaderPath, fontPath, musicPath) = do
  cWindowTitle       <- newCString windowTitle
  cWallTexturePath   <- newCString wallTexturePath
  cEnemyTexturePath  <- newCString enemyTexturePath
  cSpriteTexturePath <- newCString spriteTexturePath
  cShaderPath        <- newCString shaderPath
  cFontPath          <- newCString fontPath
  cMusicPath         <- newCString musicPath
  cInitWorkspace
    (fromIntegral windowWidth)
    (fromIntegral windowHeight)
    cWindowTitle
    (fromIntegral windowFrameLimit)
    (fromIntegral windowScale)
    cWallTexturePath
    cEnemyTexturePath
    cSpriteTexturePath
    cShaderPath
    cFontPath
    cMusicPath

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

pushDrawBuffer :: Int -> Int -> Int -> IO()
pushDrawBuffer linesCount textureType colorConstant = do
  cPushDrawBuffer
    (fromIntegral linesCount)
    (fromIntegral textureType)
    (fromIntegral colorConstant)

drawLine :: Line -> IO()
drawLine (chosenLine, xCoord, height, texCoord, color) = do
  cDrawLine
    (fromIntegral chosenLine)
    (fromIntegral xCoord)
    (realToFrac height)
    (fromIntegral texCoord)
    (fromIntegral color)

drawText :: Text -> IO()
drawText (msg, xCoord, yCoord, size) = do
  cMsg <- newCString msg
  cDrawText
    cMsg
    (fromIntegral xCoord)
    (fromIntegral yCoord)
    (fromIntegral size)

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

keyLeftArrow :: Int
keyLeftArrow = 4

keyRightArrow :: Int
keyRightArrow = 5

keyI :: Int
keyI = 6

keySpace :: Int
keySpace = 7

key1 :: Int
key1 = 8

key2 :: Int
key2 = 9

keyM :: Int
keyM = 10

keyG :: Int
keyG = 11

keyReturn :: Int
keyReturn = 12
----Color constants

colorNone :: Int
colorNone = 0

colorRed :: Int
colorRed = 1

colorGreen :: Int
colorGreen = 2

colorBlue :: Int
colorBlue = 4

----FFI calls

--Initing GUI workspace
foreign import ccall "_Z14init_workspaceiiPciiS_S_S_S_S_S_" cInitWorkspace ::
  CInt --Window width
  -> CInt --Window height
  -> CString --Window title
  -> CInt --Window frame rate limit
  -> CInt --Scale factor
  -> CString --Wall texture path
  -> CString --Enemy texture path
  -> CString --Sprite texture path
  -> CString --Shader path
  -> CString --Font path
  -> CString --Music path
  -> IO()

--Updating GUI workspace
foreign import ccall "_Z16update_workspacev" cUpdateWorkspace :: IO()

--Get delta time
foreign import ccall "_Z14get_delta_timev" cGetDeltaTime :: IO(CFloat)

--Get key pressed
foreign import ccall "_Z15get_key_pressedi" cGetKeyPressed :: CInt -> IO(Int)

--Push chosen part of draw buffer
foreign import ccall "_Z16push_draw_bufferiii" cPushDrawBuffer ::
  CInt --Lines count
  -> CInt --Texture type
  -> CInt --Color constant
  -> IO()

--Draw one line
foreign import ccall "_Z9draw_lineiidii" cDrawLine ::
  CInt --Chosen line
  -> CInt --X coord
  -> CDouble --Height
  -> CInt --Texture
  -> CInt --Color
  -> IO()

--Draw text
foreign import ccall "_Z9draw_textPciii" cDrawText ::
  CString --Msg text
  -> CInt --X coord
  -> CInt --Y coord
  -> CInt --Size
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
