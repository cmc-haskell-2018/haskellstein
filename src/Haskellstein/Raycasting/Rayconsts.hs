module Haskellstein.Raycasting.Rayconsts where

constPi :: Double
constPi = 3.141592

halfRatio :: Double
halfRatio = 0.66666666

wallTexture :: Int
wallTexture = 0

enemyTexture :: Int
enemyTexture = 1

spriteTexture :: Int
spriteTexture = 2

maxColor :: Int
maxColor = 255

darkColor :: Int
darkColor = 160

textureSize :: Int
textureSize = 64

invalidBox :: Int
invalidBox = -1

spriteTextureOffset :: Double
spriteTextureOffset = 0.0015

spriteMinDistance :: Double
spriteMinDistance = 0.35

--x, y, angle
type Camera = (Double, Double, Double)

--x, y, texture, type, offset (tmp), distance (tmp), color
type Sprite = (Double, Double, Int, Int, Double, Double, Int)
