{-# LANGUAGE DeriveFunctor #-}
module Haskellstein.Engine.TileMap where

import Haskellstein.Engine.Vector

-- | Tile coordinates.
type Coords = (Int, Int)

-- | A tile map with tiles of type @a@.
newtype TileMap a = TileMap
  { getTileMap :: Coords -> Maybe a }
  deriving (Functor)

-- | Retrieve a tile at given coordinates.
tileAt :: Coords -> TileMap a -> Maybe a
tileAt coords tm = getTileMap tm coords

-- | Find map cell coords corresponding to a given point.
--
-- >>> pointToCoords (23.4, -19.7)
-- (23,-20)
pointToCoords :: Point -> Coords
pointToCoords (x, y) = (floor x, floor y)

-- | Construct a 'TileMap' from a list of lists.
fromLists :: [[Maybe a]] -> TileMap a
fromLists xss = TileMap (\(i, j) -> (xss !! j) !! i)

-- | Construct an ASCII 'TileMap' from a list of 'String's.
asciiTileMap :: [[Char]] -> TileMap Char
asciiTileMap = fromLists . map (map f)
  where
    f ' ' = Nothing
    f c   = Just c
