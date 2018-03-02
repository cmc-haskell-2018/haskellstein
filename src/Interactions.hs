module Interactions where

import Prelude
import Data
import CheckMap

--move fireballs
stepFireballs :: [Fireball] -> Tilemap -> ([Fireball], Tilemap)
stepFireballs [] tmap      = ([], tmap)
stepFireballs (f:fs) tmap  = case newf of
                               Nothing       -> (newfs, retmap)
                               Just fireball -> (fireball : newfs, retmap)
  where
    (newf, newmap)  = stepFireball f tmap
    (newfs, retmap) = stepFireballs fs newmap

--move fireball
stepFireball :: Fireball -> Tilemap -> (Maybe Fireball, Tilemap)
stepFireball f tmap = case cond of
                        Free         -> (Just (Fireball
                                                  newx
                                                  newy
                                                  a
                                                  d
                                                  r
                                                  s
                                                  m)
                                        , tmap)
                        Blocked      -> (Nothing, tmap)
                        Destructible -> (Nothing, removeDO tmap newcoord)
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
    cond     = specCellCond tmap newcoord

--remove near Destructible objects
removeDO :: Tilemap -> CellCoord -> Tilemap
removeDO tmap (y, x) = newmap
  where
    m      = writeEmpty tmap (y, x)
    mm     = writeEmpty m (y, x + 1)
    mmm    = writeEmpty mm (y + 1, x)
    mmmm   = writeEmpty mmm (y, x - 1)
    newmap = writeEmpty mmmm (y - 1, x)

--check fireballs for hearting enemies
damageFireballs :: [Fireball] -> [Enemy] -> ([Fireball], [Enemy])
damageFireballs [] e     = ([], e)
damageFireballs (f:fs) e = case newf of
                             Nothing       -> (retlf, enret)
                             Just fireball -> (fireball : retlf, enret)
  where
    (newf, newe)   = damageFireball f e
    (retlf, enret) = damageFireballs fs newe

--check fireball for hearting enemies
damageFireball :: Fireball -> [Enemy] -> (Maybe Fireball, [Enemy])
damageFireball f []           = (Just f, [])
damageFireball f (e:es)
    | rx < r, ry < r, ehp > 0 = (Nothing, --hit
                                Enemy
                                    ex
                                    ey
                                    ehp
                                    (eDamage e)
                                    (eRange e)
                                    (eSpeed e)
                                    (eASpeed e)
                                    (eModel e)
                                    (eTex e)
                                    (eVision e)
                                    True
                                : es)
    | rx < r, ry < r          = (Nothing, es) --kill
    | otherwise               = (newf, e : enret) --miss
  where
    fx            = fPosX f
    fy            = fPosY f
    r             = fRadius f
    fd            = fDamage f
    ey            = ePosY e
    ex            = ePosX e
    ehp           = eHp e - fd
    rx            = abs (fx - ex)
    ry            = abs (fy - ey)
    (newf, enret) = damageFireball f es
