module Haskellstein.Engine where

import Control.Monad (when)
import SFML.Graphics hiding (rotate)
import SFML.Window hiding (x, y, windowWidth, windowHeight)

import Haskellstein.Engine.Raycasting

data Picture
  = PictureTexture Texture Float (Float, Float)

data EngineSettings = EngineSettings
  { engineWindowTitle     :: String
  , engineWindowSize      :: (Int, Int)
  , engineCeilingColor    :: Color
  , engineFloorColor      :: Color
  , engineFramerateLimit  :: Maybe Int
  , engineDisplayFPS      :: Bool
  , engineRaycasterDepth  :: Int
  }

defaultEngineSettings :: EngineSettings
defaultEngineSettings = EngineSettings
  { engineWindowTitle     = "Default title"
  , engineWindowSize      = (640, 480)
  , engineCeilingColor    = Color 32 32 32 255
  , engineFloorColor      = Color 64 64 64 255
  , engineFramerateLimit  = Just 60
  , engineDisplayFPS      = False
  , engineRaycasterDepth  = 1000
  }

-- | Play a game with raycasting rendering.
--
-- The game will exit on ESC.
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

  case engineFramerateLimit engineSettings of
    Nothing  -> return ()
    Just fps -> setFramerateLimit wnd fps

  Right courierFont <- fontFromFile "fonts/Courier.ttf"
  Right fpsText <- createText
  setTextFont fpsText courierFont

  timer <- createClock

  Right ceilingRectangle <- createRectangleShape
  setSize ceilingRectangle (Vec2f w (h / 2))
  setFillColor ceilingRectangle (engineCeilingColor engineSettings)

  loop timer wnd ceilingRectangle fpsText initWorld
  destroy wnd
  where
    (windowWidth, windowHeight) = engineWindowSize engineSettings
    w = fromIntegral windowWidth
    h = fromIntegral windowHeight

    loop timer wnd ceilingRectangle fpsText = go
      where
        go world = do
          evt <- pollEvent wnd
          dt <- asSeconds <$> restartClock timer

          clearRenderWindow wnd (engineFloorColor engineSettings)
          setTextString fpsText (show (1 / dt))
          drawRectangle wnd ceilingRectangle Nothing
          renderLines wnd world
          when (engineDisplayFPS engineSettings) (drawText wnd fpsText Nothing)
          display wnd

          case evt of
              Just SFEvtClosed -> return ()
              Just (SFEvtKeyPressed KeyEscape _ _ _ _) -> return ()
              Just event -> do
                go (updateWorld dt (handleEvent event world))
              _ -> go (updateWorld dt world)

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

