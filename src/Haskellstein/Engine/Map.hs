module Haskellstein.Engine.Map where

import Haskellstein.Engine.Vector

-- | Map cell coordinates.
type MapCoords = (Int, Int)

-- | Find map cell coords corresponding to a given point.
--
-- >>> pointToMapCoords (23.4, -19.7)
-- (23,-20)
pointToMapCoords :: Point -> MapCoords
pointToMapCoords (x, y) = (floor x, floor y)



