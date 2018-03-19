module Haskellstein.Raycasting where

import Foreign.C.Types
import Haskellstein.Data

--Welcome to the magic world of magic!

foreign import ccall "_Z9draw_lineiidii" cDrawLine ::
  CInt --Chosen line
  -> CInt --X coord
  -> CDouble --Height
  -> CInt --Texture
  -> CInt --Color
  -> IO()
foreign import ccall "_Z16push_draw_bufferii" cPushDrawBuffer ::
  CInt --Lines count
  -> CInt --Texture type
  -> IO()
foreign import ccall "_Z16get_window_widthv" cGetWindowWidth :: CInt
foreign import ccall "_Z17get_window_heightv" cGetWindowHeight :: CInt

constPi :: Double
constPi = 3.141592
constHalfRatio :: Double
constHalfRatio = 0.66666666

--x, y, angle
type Camera = (Double, Double, Double)
--x, y, texture, type, offset (tmp), distance (tmp)
type Sprite = (Double, Double, Int, CInt, Double, Double)

drawScene :: Scene -> IO()
drawScene scene = do
  let
    cameraStats = (
      (realToFrac $ fst $ pPos $ sPlayer scene),
      (realToFrac $ snd $ pPos $ sPlayer scene),
      (realToFrac $ pRadian $ sPlayer scene))
    intmap = tilemapToIntmap $ sTilemap scene
    fireballSprites = map (getSpriteFromFireball) (sFireball scene)
    enemySprites = map (getSpriteFromEnemy) (sEnemy scene)
    sprites = enemySprites ++ fireballSprites

  zBuffer <- rayCast cameraStats intmap
  spriteCast cameraStats sprites zBuffer
  where

    wallTexture :: CInt
    wallTexture = 0
    enemyTexture :: CInt
    enemyTexture = 1
    spriteTexture :: CInt
    spriteTexture = 2

    maxColor :: Int
    maxColor = 255
    darkColor :: Int
    darkColor = 160
    texSize :: Int
    texSize = 64
    spriteTexOffset :: Double
    spriteTexOffset = 0.0015

    windowWidth :: Int
    windowWidth = fromIntegral cGetWindowWidth

    windowHeight :: Int
    windowHeight = fromIntegral cGetWindowHeight

    invalidBox :: Int
    invalidBox = -1

    tilemapToIntmap :: Tilemap -> [[Int]]
    tilemapToIntmap tilemap = map (map cutter) tilemap
      where
        cutter :: String -> Int
        cutter str
          | head str == 'w' || head str == 'a'
            || head str == 'd' = read $ tail str
          | otherwise          = invalidBox

    getSpriteFromEnemy :: Enemy -> Sprite
    getSpriteFromEnemy enemy =
      (realToFrac $ fst $ ePos enemy, realToFrac $ snd $ ePos enemy,
        eTex enemy, enemyTexture, 0, 0)

    getSpriteFromFireball :: Fireball -> Sprite
    getSpriteFromFireball fireball =
      (realToFrac $ fst $ fPos fireball, realToFrac $ snd $fPos fireball,
        0, spriteTexture, 0, 0)

    rayCast :: Camera -> [[Int]] -> IO([Double])
    rayCast cameraStats levelMap = do
      zBuffer <- traceRay 0 cameraStats levelMap []
      cPushDrawBuffer (fromIntegral windowWidth) wallTexture
      return zBuffer

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

    traceRay :: Int -> Camera -> [[Int]] -> [Double] -> IO([Double])
    traceRay chosenX _ _ zBuffer | chosenX >= windowWidth = do
      return zBuffer
    traceRay chosenX (cameraX, cameraY, cameraAngle) lvlMap zBuffer = do
      cDrawLine (fromIntegral chosenX)
        (fromIntegral chosenX) (realToFrac height)
        (fromIntegral ((getMapElem lvlMap (fst3 collisionPoint)
          (snd3 collisionPoint)) * texSize + texX))
        (fromIntegral (maxColor - (thr3 collisionPoint)
          * (maxColor - darkColor)))
      traceRay (chosenX + 1) (cameraX, cameraY, cameraAngle) lvlMap newZBuffer
      where

        --Step by step calcs
        viewX :: Double
        viewX = (2.0 * fromIntegral chosenX)
            / (fromIntegral windowWidth) - 1
        extraAngle = cameraAngle - constPi * 0.5
        rayDirX = (cos cameraAngle) + (cos extraAngle) * constHalfRatio
            * viewX
        rayDirY = -(sin cameraAngle) - (sin extraAngle) * constHalfRatio
            * viewX
        deltaTimeX = abs (1.0 / rayDirX)
        deltaTimeY = abs (1.0 / rayDirY)
        sideStepX = makeFirstStep rayDirX cameraX
        sideStepY = makeFirstStep rayDirY cameraY
        collisionPoint =
          ddaCheck (getMapBox cameraX, getMapBox cameraY)
          sideStepX sideStepY 0
        distance =
          perpWallDist collisionPoint
            ((fst sideStepX), (fst sideStepY))
        height :: Double
        height = (fromIntegral windowHeight) / distance
        gWallX :: Double
        gWallX = calcGWallX (thr3 collisionPoint)
        lWallX :: Int
        lWallX = (floor gWallX)
        wallX :: Double
        wallX = gWallX - fromIntegral lWallX
        texWallX :: Int
        texWallX = floor $ wallX * fromIntegral texSize
        texX = calcTexX (thr3 collisionPoint)
        newZBuffer :: [Double]
        newZBuffer = distance : zBuffer

        --Used functions
        makeFirstStep :: Double -> Double -> (Int, Double)
        makeFirstStep rayDir cameraPos | rayDir < 0.0 =
          (-1, abs (((cameraPos - fromIntegral (getMapBox cameraPos))
            / rayDir)))
        makeFirstStep rayDir cameraPos =
          (1, abs ((fromIntegral (getMapBox cameraPos) + 1.0 - cameraPos)
            / rayDir))

        ddaCheck ::
          (Int, Int) --Current ray box
          -> (Int, Double) --Ray x data
          -> (Int, Double) --Ray y data
          -> Int --Chosen side
          -> (Int, Int, Int) --Collision data
        ddaCheck (mapX, mapY) _ _ side |
          (getMapElem lvlMap mapX mapY) /= invalidBox = (mapX, mapY, side)
        ddaCheck (mapX, mapY) (stepX, timeX) (stepY, timeY) _ | timeX < timeY =
          ddaCheck (mapX + stepX, mapY) (stepX, timeX + deltaTimeX)
            (stepY, timeY) 0
        ddaCheck (mapX, mapY) (stepX, timeX) (stepY, timeY) _ =
          ddaCheck (mapX, mapY + stepY) (stepX, timeX)
            (stepY, timeY + deltaTimeY) 1

        perpWallDist :: (Int, Int, Int) -> (Int, Int) -> Double
        perpWallDist (mapX, _, side) (stepX, _) |
          side == 0 = ((fromIntegral mapX) - cameraX
            + (1.0 - (fromIntegral stepX)) / 2.0) / rayDirX
        perpWallDist (_, mapY, _) (_, stepY) =
          ((fromIntegral mapY) - cameraY
            + (1.0 - (fromIntegral stepY)) / 2.0) / rayDirY

        calcGWallX :: Int -> Double
        calcGWallX 0 = cameraY + distance * rayDirY;
        calcGWallX _ = cameraX + distance * rayDirX;

        calcTexX :: Int -> Int
        calcTexX side
          | (side == 0) && (rayDirX < 0) || (side == 1) && (rayDirY > 0) =
            texSize - 1 - texWallX
          | otherwise = texWallX

    --Main sprite drawing function
    spriteCast :: Camera -> [Sprite] -> [Double] -> IO()
    spriteCast (cameraX, cameraY, cameraAngle) spriteList zBuffer = do
      let
        newList = map (calcSpriteDistance) spriteList
        sortList = sortSprites newList
        endList = deleteSprites sortList
      drawSprites endList
      where

        calcSpriteDistance :: Sprite -> Sprite
        calcSpriteDistance (spriteX, spriteY, texture, spriteType, _, _) =
          let
            spriteCameraX = spriteX - cameraX
            spriteCameraY = spriteY - cameraY
            angle = cameraAngle
            offset = (spriteCameraX * (sin angle)
              + spriteCameraY * (cos angle)) / constHalfRatio
            distance = spriteCameraX * (cos angle)
              + spriteCameraY * (-(sin angle))
          in (spriteX, spriteY, texture, spriteType, offset, distance)

        getDistance :: Sprite -> Double
        getDistance (_, _, _, _, _, distance) = distance

        sortSprites :: [Sprite] -> [Sprite]
        sortSprites [] = []
        sortSprites (spriteHead : spriteTail) =
          sortSprites [sprite | sprite <- spriteTail,
            (getDistance sprite) >= (getDistance spriteHead)] ++
          [spriteHead] ++
          sortSprites [sprite | sprite <- spriteTail,
            (getDistance sprite) < (getDistance spriteHead)]

        minDistance :: Double
        minDistance = 0.3

        deleteSprites :: [Sprite] -> [Sprite]
        deleteSprites [] = []
        deleteSprites (spriteHead : spriteTail)
          | (getDistance spriteHead) < minDistance = deleteSprites spriteTail
          | otherwise = spriteHead : (deleteSprites spriteTail)

        drawSprites :: [Sprite] -> IO()
        drawSprites [] = do
          return ()
        drawSprites sprList = do
          drawSprite $ head sprList
          drawSprites $ tail sprList
          where

            drawSprite :: Sprite -> IO()
            drawSprite (_, _, texture, spriteType, offset, distance) = do
              stripeCount <- drawSpriteStripe spriteStartX 0
              cPushDrawBuffer (fromIntegral stripeCount) spriteType
              where

                spriteScreenX :: Double
                spriteScreenX = ((fromIntegral windowWidth) / 2)
                  * (1.0 + offset / distance)
                spriteSize :: Double
                spriteSize = abs $ (fromIntegral windowHeight) / distance
                halfSpriteSize :: Double
                halfSpriteSize = spriteSize / 2
                spriteStartX :: Int
                spriteStartX = floor (-halfSpriteSize + spriteScreenX)
                spriteEndX :: Int
                spriteEndX = floor (halfSpriteSize + spriteScreenX)

                drawSpriteStripe :: Int -> Int -> IO(Int)
                drawSpriteStripe spriteX stripeCount
                  | spriteX < spriteEndX = do
                    doneStripe <- endSpriteStripe stripeCount
                    realCount <- drawSpriteStripe (spriteX + 1) (stripeCount
                      + doneStripe)
                    return realCount
                  | otherwise = do return stripeCount
                  where

                    localStripe :: Double
                    localStripe = (fromIntegral spriteX) - (-halfSpriteSize
                      + spriteScreenX)
                    spriteTexX :: Double
                    spriteTexX = (localStripe / spriteSize
                      + spriteTexOffset) * (fromIntegral texSize)
                    sprTexX :: Int
                    sprTexX | spriteTexX >= (fromIntegral texSize) =
                              texSize - 1
                            | spriteTexX < 0 = ceiling spriteTexX
                            | otherwise      = floor spriteTexX

                    endSpriteStripe :: Int -> IO(Int)
                    endSpriteStripe stripeId | distance > 0
                      && spriteX >= 0
                      && spriteX < windowWidth
                      && (getZBufferValue spriteX) >= distance = do
                        cDrawLine (fromIntegral stripeId)
                          (fromIntegral spriteX) (realToFrac spriteSize)
                          (fromIntegral (texture * texSize + sprTexX))
                          (fromIntegral maxColor)
                        return 1
                          | otherwise = do return 0
                        where
                          getZBufferValue :: Int -> Double
                          getZBufferValue chosenLine = head $ drop
                            (windowWidth - chosenLine - 1) zBuffer
