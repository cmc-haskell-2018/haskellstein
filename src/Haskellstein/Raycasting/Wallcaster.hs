module Haskellstein.Raycasting.Wallcaster where

import Haskellstein.Sftool
import Haskellstein.Raycasting.Rayconsts

----Shell functions

wallCast :: Camera -> [[Int]] -> IO([Double])
wallCast cameraStats levelMap = do
  zBuffer <- traceRay 0 cameraStats levelMap []
  pushDrawBuffer getLinesCount wallTexture
  return zBuffer

----Rendering functions

traceRay :: Int -> Camera -> [[Int]] -> [Double] -> IO([Double])
traceRay chosenX _ _ zBuffer | chosenX >= getLinesCount = do
  return zBuffer
traceRay chosenX (cameraX, cameraY, cameraAngle) lvlMap zBuffer = do
  drawLine
    (chosenX, chosenX, height,
      ((getMapElem lvlMap (fst3 collisionPoint)
        (snd3 collisionPoint)) * textureSize + texX),
      (maxColor - (thr3 collisionPoint) * (maxColor - darkColor)))
  traceRay (chosenX + 1) (cameraX, cameraY, cameraAngle) lvlMap newZBuffer
  where

    viewX :: Double
    viewX = (2.0 * fromIntegral chosenX) / (fromIntegral getLinesCount) - 1

    extraAngle ::  Double
    extraAngle = cameraAngle + constPi * 0.5

    rayDirX :: Double
    rayDirX = (cos cameraAngle) + (cos extraAngle)
      * halfRatio * viewX

    rayDirY :: Double
    rayDirY = (sin cameraAngle) + (sin extraAngle)
      * halfRatio * viewX

    deltaTimeX :: Double
    deltaTimeX = abs (1.0 / rayDirX)

    deltaTimeY :: Double
    deltaTimeY = abs (1.0 / rayDirY)

    sideStepX :: (Int, Double)
    sideStepX = makeFirstStep rayDirX cameraX

    sideStepY :: (Int, Double)
    sideStepY = makeFirstStep rayDirY cameraY

    collisionPoint :: (Int, Int, Int)
    collisionPoint =
      ddaCheck lvlMap (getMapBox cameraX, getMapBox cameraY)
        sideStepX sideStepY (deltaTimeX, deltaTimeY) 0

    distance :: Double
    distance = perpWallDist (cameraX, cameraY) (rayDirX, rayDirY)
      collisionPoint ((fst sideStepX), (fst sideStepY))

    height :: Double
    height = (fromIntegral getWindowHeight) / distance

    gWallX :: Double
    gWallX = calcGWallX (cameraX, cameraY) (rayDirX, rayDirY)
      distance (thr3 collisionPoint)

    lWallX :: Int
    lWallX = (floor gWallX)

    wallX :: Double
    wallX = gWallX - fromIntegral lWallX

    texWallX :: Int
    texWallX = floor $ wallX * fromIntegral textureSize

    texX :: Int 
    texX = calcTexX (rayDirX, rayDirY) texWallX (thr3 collisionPoint)

    newZBuffer :: [Double]
    newZBuffer = distance : zBuffer

fst3 :: (a, a, a) -> a
fst3 (first, _, _) = first

snd3 :: (a, a, a) -> a
snd3 (_, second, _) = second

thr3 :: (a, a, a) -> a
thr3 (_, _, third) = third

getMapBox :: Double -> Int
getMapBox pos = floor pos

getMapElem :: [[Int]] -> Int -> Int -> Int
getMapElem lvlMap x y = head $ drop x $ head $ drop y lvlMap

makeFirstStep :: Double -> Double -> (Int, Double)
makeFirstStep rayDir cameraPos | rayDir < 0.0 =
  (-1, abs (((cameraPos - fromIntegral (getMapBox cameraPos)) / rayDir)))
makeFirstStep rayDir cameraPos =
  (1, abs ((fromIntegral (getMapBox cameraPos) + 1.0 - cameraPos) / rayDir))

ddaCheck ::
  [[Int]] --Level map
  -> (Int, Int) --Current ray box
  -> (Int, Double) --Ray x data
  -> (Int, Double) --Ray y data
  -> (Double, Double)
  -> Int --Chosen side
  -> (Int, Int, Int) --Collision data
ddaCheck lvlMap (mapX, mapY) _ _ _ side |
  (getMapElem lvlMap mapX mapY) /= invalidBox = (mapX, mapY, side)
ddaCheck lvlMap (mapX, mapY) (stepX, timeX) (stepY, timeY)
  (deltaX, deltaY) _ | timeX < timeY =
    ddaCheck lvlMap (mapX + stepX, mapY) (stepX, timeX + deltaX) (stepY, timeY)
      (deltaX, deltaY) 0
ddaCheck lvlMap (mapX, mapY) (stepX, timeX) (stepY, timeY)
  (deltaX, deltaY) _ =
    ddaCheck lvlMap (mapX, mapY + stepY) (stepX, timeX) (stepY, timeY + deltaY)
      (deltaX, deltaY) 1

perpWallDist ::
  (Double, Double) --Camera's position
  -> (Double, Double) --Ray's direction
  -> (Int, Int, Int) --Collision point
  -> (Int, Int) --Steps
  -> Double --Distance
perpWallDist (posX, _) (rayDirX, _) (mapX, _, side) (stepX, _) | side == 0 =
    ((fromIntegral mapX) - posX + (1.0 - (fromIntegral stepX)) / 2) / rayDirX
perpWallDist (_, posY) (_, rayDirY) (_, mapY, _) (_, stepY) =
  ((fromIntegral mapY) - posY + (1.0 - (fromIntegral stepY)) / 2) / rayDirY

calcGWallX :: (Double, Double) -> (Double, Double) -> Double -> Int -> Double
calcGWallX (_, posY) (_, rayDirY) distance 0 = posY + distance * rayDirY;
calcGWallX (posX, _) (rayDirX, _) distance _ = posX + distance * rayDirX;

calcTexX :: (Double, Double) -> Int -> Int -> Int
calcTexX (rayDirX, rayDirY) texWallX side
  | (side == 0) && (rayDirX < 0) || (side == 1) && (rayDirY > 0) =
    textureSize - 1 - texWallX
  | otherwise =
    texWallX
