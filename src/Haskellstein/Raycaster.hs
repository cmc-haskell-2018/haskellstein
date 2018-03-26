module Haskellstein.Raycaster where

import Haskellstein.Data
import Haskellstein.Raycasting.Wallcaster
import Haskellstein.Raycasting.Spritecaster
import Haskellstein.Raycasting.Rayconsts
 
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
  zBuffer <- wallCast cameraStats intmap
  spriteCast cameraStats sprites zBuffer

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
