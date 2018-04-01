module Haskellstein.Engine.Picture where

import SFML.Graphics (Texture)

data Picture
  = PictureTexture Texture Float (Float, Float)

