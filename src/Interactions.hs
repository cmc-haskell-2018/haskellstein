module Interactions where

import Prelude
import Data
import CheckMap

--move fireballs
stepFireballs :: [Fireball] -> Tilemap -> ([Fireball], Tilemap)
stepFireballs (f:fs) map =


--move fireball
interactFireball :: Fireball -> Tilemap -> (Maybe Fireball, Tilemap)
interactFireball f map = case cond of
                           Free        -> (Just Fireball
                                                newx
                                                newy
                                                a
                                                d
                                                r
                                                s
                                                m
                                                , map)
                           Blocked      -> (Nothing, map)
                           Destructible -> (Nothing, removeDO map newcoord)
  where
    x        = fPosX f
    y        = fPosY f
    a        = fRadian f
    s        = fSpeed f
    d        = fDamage f
    m        = fModel f
    r        = fRadius f
    newx     = x + (s * cos a)
    newy     = y + (s * sin a)
    newcoord = (floor newy, floor newx)
    cond     = specCellCoord map newcoord

--remove near Destructible objects
removeDO :: Tilemap -> CellCoord -> Tilemap
removeDO map (y, x) = newmap
  where
    m      = write (y, x) map
    mm     = write (y, x + 1) m
    mmm    = write (y + 1, x) mm
    mmmm   = write (y, x - 1) mmm
    newmap = write (y - 1, x) mmmm
