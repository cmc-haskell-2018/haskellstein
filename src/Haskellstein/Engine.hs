module Haskellstein.Engine (
  -- * Re-exports
  module Haskellstein.Engine.Camera,
  module Haskellstein.Engine.Map,
  module Haskellstein.Engine.Picture,
  module Haskellstein.Engine.Settings,

  -- * Pure game interface
  play,
) where

import Control.Monad (when)
import SFML.Graphics hiding (rotate)
import SFML.Window hiding (x, y, windowWidth, windowHeight)

import Haskellstein.Engine.Camera
import Haskellstein.Engine.Map
import Haskellstein.Engine.Picture
import Haskellstein.Engine.Render
import Haskellstein.Engine.Settings

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
  -- create window
  wnd <- createRenderWindow
            (VideoMode windowWidth windowHeight 32)
            (engineWindowTitle engineSettings)
            [SFDefaultStyle]
            (Just defaultContextSettings)

  -- set framerate limit if necessary
  case engineFramerateLimit engineSettings of
    Nothing  -> return ()
    Just fps -> setFramerateLimit wnd fps

  -- create FPS text
  Right courierFont <- fontFromFile "fonts/Courier.ttf"
  Right fpsText <- createText
  setTextFont fpsText courierFont

  -- create ceiling rectangle
  Right ceilingRectangle <- createRectangleShape
  setSize ceilingRectangle (Vec2f w (h / 2))
  setFillColor ceilingRectangle (engineCeilingColor engineSettings)

  -- create timer
  timer <- createClock

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

          -- render everything
          clearRenderWindow wnd (engineFloorColor engineSettings)
          setTextString fpsText (show (1 / dt))
          drawRectangle wnd ceilingRectangle Nothing
          renderVerticalLines wnd
            (engineWindowSize engineSettings)
            (engineRaycasterDepth engineSettings)
            (worldCamera world)
            (worldMap world)
          when (engineDisplayFPS engineSettings) (drawText wnd fpsText Nothing)
          display wnd

          -- handle events
          case evt of
              Just SFEvtClosed -> return ()
              Just (SFEvtKeyPressed KeyEscape _ _ _ _) -> return ()  -- exit on ESC
              Just event -> do
                go (updateWorld dt (handleEvent event world))
              _ -> go (updateWorld dt world)

defaultContextSettings :: ContextSettings
defaultContextSettings = ContextSettings
  { depthBits         = 24
  , stencilBits       = 8
  , antialiasingLevel = 0
  , majorVersion      = 1
  , minorVersion      = 2
  , attributeFlags    = [ContextDefault]
  }

