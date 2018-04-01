module Haskellstein.Engine where

import SFML.Graphics hiding (rotate)
import SFML.Window hiding (x, y, windowWidth, windowHeight)

import Haskellstein.Engine.Raycasting

data Picture
  = PictureTexture Texture Float (Float, Float)

-- | Play a game with raycasting rendering.
play
  :: String                                 -- ^ Window title.
  -> (Int, Int)                             -- ^ Window size.
  -> Color                                  -- ^ Background color.
  -> Int                                    -- ^ Raycaster depth.
  -> world                                  -- ^ Initial world state.
  -> (world -> MapCoords -> Maybe Picture)  -- ^ World map.
  -> (world -> Camera)                      -- ^ Player camera.
  -> (SFEvent -> world -> world)            -- ^ Event handler.
  -> IO ()
play title windowSize bgColor raycasterDepth initWorld worldMap worldCamera handleEvent = do
  wnd <- createRenderWindow
            (VideoMode windowWidth windowHeight 32)
            title
            [SFDefaultStyle]
            (Just defaultContextSettings)
  loop wnd initWorld
  destroy wnd
  where
    (windowWidth, windowHeight) = windowSize

    loop wnd world = do
      clearRenderWindow wnd bgColor
      renderLines wnd world
      display wnd

      evt <- waitEvent wnd
      case evt of
          Just SFEvtClosed -> return ()
          Just event -> loop wnd (handleEvent event world)
          _ -> loop wnd world

    renderLines wnd world =
      mapM_ (\(i, hs) -> mapM_ (renderWall i) (take 1 hs))
        (zip [0..] (raycastWithMap raycasterDepth windowWidth sampleMap camera))
      where
        camera = worldCamera world
        sampleMap = worldMap world

        renderWall x (Hit info (PictureTexture wallTexture offset (texW, texH))) = do
          drawPrimitives wnd 
            [ Vertex (Vec2f x (h / 2 - y)) wallColor (Vec2f texX 0)
            , Vertex (Vec2f x (h / 2 + y)) wallColor (Vec2f texX (texH - 1))
            ] Lines (Just renderStates { texture = wallTexture })
          where
            h = fromIntegral windowHeight
            y = h / 2 / hitDistance info

            texX = offset + hitPosition info * (texW - 1)

            wallColor =
              case hitSide info of
                SideX -> Color 255 255 255 255
                SideY -> Color 128 128 128 255
                          
defaultContextSettings :: ContextSettings
defaultContextSettings = ContextSettings
  { depthBits         = 24
  , stencilBits       = 8
  , antialiasingLevel = 0
  , majorVersion      = 1
  , minorVersion      = 2
  , attributeFlags    = [ContextDefault]
  }

