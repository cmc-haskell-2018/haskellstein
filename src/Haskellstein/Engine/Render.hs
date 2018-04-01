module Haskellstein.Engine.Render where

import SFML.Graphics
import SFML.Window hiding (x, y, windowWidth, windowHeight)

import Haskellstein.Engine.Camera
import Haskellstein.Engine.Picture
import Haskellstein.Engine.Raycasting
import Haskellstein.Engine.TileMap

-- | Render vertical lines produced by a raycaster.
renderVerticalLines
  :: RenderWindow     -- ^ Window to render onto.
  -> (Int, Int)       -- ^ Screen size.
  -> Int              -- ^ Maximum raycasting depth (in cells).
  -> Camera           -- ^ Raycasting camera.
  -> TileMap Picture  -- ^ World map.
  -> IO ()
renderVerticalLines wnd (windowWidth, windowHeight) depth camera m =
  mapM_ (\(i, hs) -> mapM_ (renderWall i) (take 1 hs))
    (zip [0..] (raycastWithTileMap depth windowWidth m camera))
  where
    h = fromIntegral windowHeight

    renderWall x (Hit info (PictureTexture wallTexture offset (texW, texH))) = do
      drawPrimitives wnd 
        [ Vertex (Vec2f x (h / 2 - y)) wallColor (Vec2f texX 0)
        , Vertex (Vec2f x (h / 2 + y)) wallColor (Vec2f texX (texH - 1))
        ] Lines (Just renderStates { texture = wallTexture })
      where
        y = h / 2 / hitDistance info

        texX = offset + hitPosition info * (texW - 1)

        wallColor =
          case hitSide info of
            SideX -> Color 255 255 255 255
            SideY -> Color 128 128 128 255
                          

