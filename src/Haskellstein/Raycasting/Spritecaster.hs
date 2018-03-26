module Haskellstein.Raycasting.Spritecaster where

import Haskellstein.Sftool
import Haskellstein.Raycasting.Rayconsts

----Shell functions

spriteCast :: Camera -> [Sprite] -> [Double] -> IO()
spriteCast (cameraX, cameraY, cameraAngle) spriteList zBuffer = do
  let
    newList = map (calcSpriteDistance) spriteList
    sortList = sortSprites newList
    endList = deleteSprites sortList
  drawSprites endList zBuffer
  where

    calcSpriteDistance :: Sprite -> Sprite
    calcSpriteDistance (spriteX, spriteY, texture, spriteType, _, _) =
      let
        spriteCameraX = spriteX - cameraX
        spriteCameraY = spriteY - cameraY
        angle = -cameraAngle
        raw_off = (spriteCameraX * (sin angle) + spriteCameraY * (cos angle))
        offset = raw_off / halfRatio
        distance = spriteCameraX * (cos angle) + spriteCameraY * (-(sin angle))
      in (spriteX, spriteY, texture, spriteType, offset, distance)

----Rendering functions

sortSprites :: [Sprite] -> [Sprite]
sortSprites [] = []
sortSprites (spriteHead : spriteTail) =
  sortSprites [sprite | sprite <- spriteTail,
    (getDistance sprite) >= (getDistance spriteHead)]
  ++ [spriteHead]
  ++ sortSprites [sprite | sprite <- spriteTail,
    (getDistance sprite) < (getDistance spriteHead)]

deleteSprites :: [Sprite] -> [Sprite]
deleteSprites [] = []
deleteSprites (spriteHead : spriteTail)
  | (getDistance spriteHead) < spriteMinDistance = deleteSprites spriteTail
  | otherwise = spriteHead : (deleteSprites spriteTail)

getDistance :: Sprite -> Double
getDistance (_, _, _, _, _, distance) = distance

drawSprites :: [Sprite] -> [Double] -> IO()
drawSprites [] _ = do
  return ()
drawSprites sprList zBuffer = do
  drawSprite (head sprList) zBuffer
  drawSprites (tail sprList) zBuffer

type SpriteInfo = (Double, Double, Double, Int, Int)
 
drawSprite :: Sprite -> [Double] -> IO()
drawSprite (_, _, texture, spriteType, offset, distance) zBuffer = do
  stripeCount <- drawSpriteStripe
    (texture, distance)
    (spriteScreenX, spriteHeight, spriteWidth, spriteStartX, spriteEndX)
    spriteStartX 0 zBuffer
  pushDrawBuffer stripeCount spriteType
  where
    spriteScreenX :: Double
    spriteScreenX = ((fromIntegral getLinesCount) / 2)
      * (1.0 + offset / distance)

    spriteHeight :: Double
    spriteHeight = abs $ (fromIntegral getWindowHeight) / distance

    spriteWidth :: Double
    spriteWidth = spriteHeight / (realToFrac getScaleFactor)

    spriteStartX :: Int
    spriteStartX = floor (-(spriteWidth / 2) + spriteScreenX)

    spriteEndX :: Int
    spriteEndX = floor (spriteWidth / 2 + spriteScreenX)

drawSpriteStripe ::
  (Int, Double) --Sprite stats
  -> SpriteInfo --Current sprite info
  -> Int --Sprite chosen x
  -> Int --Count of stripes
  -> [Double] --Depth zBuffer
  -> IO(Int) --Real count of stripes
drawSpriteStripe
  (texture, distance)
  (spriteScreenX, spriteHeight, spriteWidth, spriteStartX, spriteEndX)
  spriteX stripeCount zBuffer
    | spriteX < spriteEndX = do
      doneStripe <- endSpriteStripe
        (texture, distance)
        (spriteScreenX, spriteHeight, spriteWidth, spriteStartX, spriteEndX)
        spriteX stripeCount zBuffer
      realCount <- drawSpriteStripe
        (texture, distance)
        (spriteScreenX, spriteHeight, spriteWidth, spriteStartX, spriteEndX)
        (spriteX + 1) (stripeCount + doneStripe) zBuffer
      return realCount
    | otherwise = do return stripeCount

endSpriteStripe ::
  (Int, Double) --Sprite stats
  -> SpriteInfo --Current sprite info
  -> Int --Sprite chosen x
  -> Int --Stripe id
  -> [Double] --Depth zBuffer
  -> IO(Int) --Stripe success
endSpriteStripe
  (texture, distance)
  (spriteScreenX, spriteHeight, spriteWidth, _, _)
  spriteX stripeId zBuffer
  | distance > 0
    && spriteX >= 0
    && spriteX < getLinesCount
    && (getZBufferValue spriteX) >= distance = do
      drawLine (stripeId, spriteX, spriteHeight,
        (texture * textureSize + sprTexX spriteX spriteScreenX spriteWidth),
        maxColor)
      return 1
  | otherwise = do return 0
  where

    getZBufferValue :: Int -> Double
    getZBufferValue chosenLine =
      head $ drop (getLinesCount - chosenLine - 1) zBuffer

sprTexX :: Int -> Double -> Double -> Int
sprTexX spriteX spriteScreenX spriteWidth
  | spriteTexX >= (fromIntegral textureSize) = textureSize - 1
  | spriteTexX < 0 = ceiling spriteTexX
  | otherwise      = floor spriteTexX
  where

    spriteTexX :: Double
    spriteTexX = (localStripe / spriteWidth + spriteTextureOffset)
      * (fromIntegral textureSize)

    localStripe :: Double
    localStripe = (fromIntegral spriteX) - (-(spriteWidth / 2) + spriteScreenX)
