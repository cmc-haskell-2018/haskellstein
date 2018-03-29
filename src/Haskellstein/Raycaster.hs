module Haskellstein.Raycaster where

import Haskellstein.Picture
import Haskellstein.Raycasting.Wallcaster
import Haskellstein.Raycasting.Spritecaster
 
drawScene :: Picture -> IO()
drawScene picture = do
  let
    cameraStats = piCamera picture
    intmap      = piIntmap picture
    sprites     = piSprite picture
  zBuffer       <- wallCast cameraStats intmap
  spriteCast cameraStats sprites zBuffer
