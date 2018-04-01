module Haskellstein.Engine where

import SFML.Graphics hiding (rotate)
import SFML.Window hiding (x, y, windowWidth, windowHeight)

import Haskellstein.Engine.Raycasting

data Picture
  = PictureTexture Texture Float (Float, Float)

data EngineSettings = EngineSettings
  { engineWindowTitle     :: String
  , engineWindowSize      :: (Int, Int)
  , engineBackgroundColor :: Color
  , engineRaycasterDepth  :: Int
  }

defaultEngineSettings :: EngineSettings
defaultEngineSettings = EngineSettings
  { engineWindowTitle     = "Default title"
  , engineWindowSize      = (640, 480)
  , engineBackgroundColor = Color 0 0 0 255
  , engineRaycasterDepth  = 1000
  }

-- | Play a game with raycasting rendering.
play
  :: EngineSettings
  -> world                                  -- ^ Initial world state.
  -> (world -> MapCoords -> Maybe Picture)  -- ^ World map.
  -> (world -> Camera)                      -- ^ Player camera.
  -> (SFEvent -> world -> world)            -- ^ Event handler.
  -> (Float -> world -> world)              -- ^ Update function (to be called every frame).
  -> IO ()
play engineSettings initWorld worldMap worldCamera handleEvent updateWorld = do
  wnd <- createRenderWindow
            (VideoMode windowWidth windowHeight 32)
            (engineWindowTitle engineSettings)
            [SFDefaultStyle]
            (Just defaultContextSettings)
  timer <- createClock
  loop timer wnd initWorld
  destroy wnd
  where
    (windowWidth, windowHeight) = engineWindowSize engineSettings

    loop timer wnd world = do
      clearRenderWindow wnd (engineBackgroundColor engineSettings)
      renderLines wnd world
      display wnd

      evt <- pollEvent wnd
      dt <- asSeconds <$> restartClock timer
      case evt of
          Just SFEvtClosed -> return ()
          Just event -> do
            loop timer wnd (updateWorld dt (handleEvent event world))
          _ -> loop timer wnd (updateWorld dt world)

    renderLines wnd world =
      mapM_ (\(i, hs) -> mapM_ (renderWall i) (take 1 hs))
        (zip [0..] (raycastWithMap depth windowWidth sampleMap camera))
      where
        depth = engineRaycasterDepth engineSettings
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

