module Haskellstein.Picture where

import Haskellstein.Data
import Haskellstein.Raycasting.Rayconsts

data Picture = Picture
  { piSprite :: [Sprite]
  , piIntmap :: [[Int]]
  , piCamera :: Camera
  , piHp     :: Int
  }

--sceneToPicture
makePicture :: Scene -> Picture
makePicture scene = Picture sprites intmap camera hp
  where
    spriteEnemy      = map getSpriteFromEnemy (sEnemy scene)
    spriteDeadEnemy  = map getSpriteFromEnemy (sDeadEnemy scene)
    spriteFireballP  = map getSpriteFromFireball (sFireball scene)
    spriteFireballE  = map getSpriteFromFireball (sEnemyFireball scene)
    spriteFireball   = spriteFireballP ++ spriteFireballE
    spriteObject     = tilemapToSprite (sTilemap scene) 0
    sprites          = spriteEnemy
                       ++ spriteFireball
                       ++ spriteObject
                       ++ spriteDeadEnemy
    intmap           = tilemapToIntmap (sTilemap scene)
    camera           = ( (realToFrac $ fst $ pPos $ sPlayer scene)
                       , (realToFrac $ snd $ pPos $ sPlayer scene)
                       , (realToFrac $ pRadian $ sPlayer scene))
    hp               = pHp $ sPlayer scene

--extractWalls
tilemapToIntmap :: Tilemap -> [[Int]]
tilemapToIntmap tilemap = map (map cutter) tilemap
  where
    cutter :: String -> Int
    cutter str
      | head str == 'w' || head str == 'a'
        || head str == 'd' = read $ tail str
      | otherwise          = invalidBox

--extractSprites
tilemapToSprite :: Tilemap -> Int -> [Sprite]
tilemapToSprite [] _         = []
tilemapToSprite (str:rest) y = (strsToSprite str y 0)
                               ++ (tilemapToSprite rest (y + 1))

--extractSpritesSubTilemap
strsToSprite :: [String] -> Int -> Int -> [Sprite]
strsToSprite [] _ _ = []
strsToSprite (str : rest) y x
    | exist         = (createSpriteStr str y x) : (strsToSprite rest y (x + 1))
    | otherwise     = strsToSprite rest y (x + 1)
  where
    exist  = (head str == 'o'|| head str == 'i' || head str == 'b')

--create Sprite from str
createSpriteStr :: String -> Int -> Int -> Sprite
createSpriteStr str y x = 
  ((fromIntegral x) + 0.5, (fromIntegral y) + 0.5,
    read $ tail str, spriteTexture, 0, 0)

--makeSpritesFromEnemy
getSpriteFromEnemy :: Enemy -> Sprite
getSpriteFromEnemy enemy =
  (realToFrac $ fst $ ePos enemy, realToFrac $ snd $ ePos enemy,
    eTex enemy, enemyTexture, 0, 0)

--makeSpritesFromFireball
getSpriteFromFireball :: Fireball -> Sprite
getSpriteFromFireball fireball =
  (realToFrac $ fst $ fPos fireball, realToFrac $ snd $fPos fireball,
    fTex fireball, spriteTexture, 0, 0)
