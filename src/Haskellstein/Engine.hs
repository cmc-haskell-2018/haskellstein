module Haskellstein.Engine where

import SFML.Window

run :: IO ()
run = do
  wnd <- createWindow
            defaultVideoMode
            "Haskellstein"
            [SFDefaultStyle]
            (Just defaultContextSettings)
  loop wnd
  destroy wnd

defaultVideoMode :: VideoMode
defaultVideoMode = VideoMode
  { windowWidth  = 640
  , windowHeight = 480
  , windowBPP    = 32
  }

defaultContextSettings :: ContextSettings
defaultContextSettings = ContextSettings
  { depthBits         = 24
  , stencilBits       = 8
  , antialiasingLevel = 0
  , majorVersion      = 1
  , minorVersion      = 2
  , attributeFlags    = [ContextDefault]
  }

loop :: Window -> IO ()
loop wnd = do
    evt <- waitEvent wnd
    case evt of
        Just SFEvtClosed -> return ()
        _ -> loop wnd
