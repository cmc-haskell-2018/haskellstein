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
    spriteEnemy    = map getSpriteFromEnemy (sEnemy scene)
    spriteFireball = map getSpriteFromFireball (sFireball scene)
    sprites        = spriteEnemy ++ spriteFireball
    intmap         = tilemapToIntmap (sTilemap scene)
    camera         = ( (realToFrac $ fst $ pPos $ sPlayer scene)
                     , (realToFrac $ snd $ pPos $ sPlayer scene)
                     , (realToFrac $ pRadian $ sPlayer scene))
    hp             = pHp $ sPlayer scene

--extractWalls 
tilemapToIntmap :: Tilemap -> [[Int]]
tilemapToIntmap tilemap = map (map cutter) tilemap
  where
    cutter :: String -> Int
    cutter str
      | head str == 'w' || head str == 'a'
        || head str == 'd' = read $ tail str
      | otherwise          = invalidBox

--makeSpritesFromEnemy
getSpriteFromEnemy :: Enemy -> Sprite
getSpriteFromEnemy enemy =
  (realToFrac $ fst $ ePos enemy, realToFrac $ snd $ ePos enemy,
    eTex enemy, enemyTexture, 0, 0)

--makeSpritesFromFireball
getSpriteFromFireball :: Fireball -> Sprite
getSpriteFromFireball fireball =
  (realToFrac $ fst $ fPos fireball, realToFrac $ snd $fPos fireball,
    0, spriteTexture, 0, 0)
