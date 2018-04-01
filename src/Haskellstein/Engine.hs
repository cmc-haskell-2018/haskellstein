{-# OPTIONS_GHC -fno-warn-type-defaults #-}
module Haskellstein.Engine where

import SFML.Graphics hiding (rotate)
import SFML.Window hiding (x, y)

import System.IO.Unsafe

import Haskellstein.Engine.Raycasting

wallTexture :: Texture
wallTexture = unsafePerformIO $ do
  Right t <- textureFromFile "textures/wall.png" Nothing
  return t

run :: IO ()
run = do
  wnd <- createRenderWindow
            defaultVideoMode
            "Haskellstein"
            [SFDefaultStyle]
            (Just defaultContextSettings)
  let theta = -0.3
      dir = rotate theta (-1, 0)
      plane = rotate (theta + pi/2) (-0.66, 0)
      
  loop wnd (Camera (16, 1.5) dir plane)
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

renderLines :: Camera -> [Vertex]
renderLines camera =
  foldMap (\(i, hs) -> concatMap (renderWall i) (take 1 hs))
    (zip [0..] (raycastWithMap 100 640 sampleMap camera))
  where
    renderWall i (Hit info c) = 
      [ Vertex (Vec2f x (240 - y)) wallColor (Vec2f texX' 0)
      , Vertex (Vec2f x (240 + y)) wallColor (Vec2f texX' 63)
      ]
      where
        x = fromIntegral i
        y = 240 / hitDistance info

        texX = hitPosition info * 63

        texX'
          | c == '#' = texX
          | otherwise = texX + 64

        wallColor =
          case hitSide info of
            SideX -> Color 255 255 255 255
            SideY -> Color 128 128 128 255
                      

    sampleMap (i, j) = (xs !! j) !! i
      where
        xs = map (map f)
          [ "#@#@#@#@#@#@#@#@#@"
          , "#                #"
          , "#@#@#@#          #"
          , "#   #     #      #"
          , "#   #     #      #"
          , "#   #@#@#@# #@#@#@"
          , "#      #         #"
          , "#           #    #"
          , "#@#@#@#@#@#@#@#@#@"
          ]
    
        f ' ' = Nothing
        f c = Just c

loop :: RenderWindow -> Camera -> IO ()
loop wnd camera = do
    clearRenderWindow wnd (Color 32 32 32 255)
    drawPrimitives wnd (renderLines camera) Lines (Just renderStates { texture = wallTexture })
    display wnd
    evt <- waitEvent wnd
    case evt of
        Just SFEvtClosed -> return ()
        Just (SFEvtKeyPressed KeyA _ _ _ _) ->
          loop wnd (rotateCamera (-0.1) camera)
        Just (SFEvtKeyPressed KeyD _ _ _ _) ->
          loop wnd (rotateCamera 0.1 camera)
        Just (SFEvtKeyPressed KeyW _ _ _ _) ->
          loop wnd (moveCamera 0.1 camera)
        Just (SFEvtKeyPressed KeyS _ _ _ _) ->
          loop wnd (moveCamera (-0.1) camera)
        _ -> loop wnd camera
