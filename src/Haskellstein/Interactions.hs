module Haskellstein.Interactions where

import Prelude
import Haskellstein.Data
import Haskellstein.CheckMap
import Haskellstein.Initialize

-------------------------SHELL_FUNCTIONS------------------------------------

--all interactions
doInteractions :: Scene -> Scene
doInteractions = doEnemies . doFireballs . doPlayer

--all actions of fireballs
doFireballs :: Scene -> Scene
doFireballs = sStepFireballs . sDamageFireballs

sStepFireballs :: Scene -> Scene
sStepFireballs scene =
    Scene
        (sPlayer scene)
        newf
        (sEnemy scene)
        newtmap
        (sControl scene)
        (sDelta scene)
  where
    (newf, newtmap) = stepFireballs (sFireball scene)
                                    (sTilemap scene)
                                    (sDelta scene)

sDamageFireballs :: Scene -> Scene
sDamageFireballs scene =
    Scene
        (sPlayer scene)
        newf
        newe
        (sTilemap scene)
        (sControl scene)
        (sDelta scene)
  where
    (newf, newe) = damageFireballs (sFireball scene) (sEnemy scene)

--all actions of enemies
doEnemies :: Scene -> Scene
doEnemies = sStepEnemies

sStepEnemies :: Scene -> Scene
sStepEnemies scene =
    Scene
        newp
        (sFireball scene)
        newe
        (sTilemap scene)
        (sControl scene)
        (sDelta scene)
  where
    (newp, newe) = stepEnemies (sPlayer scene)
                               (sEnemy scene)
                               (sTilemap scene)
                               (sDelta scene)

--all actions of player
doPlayer :: Scene -> Scene
doPlayer = sControlPlayer

sControlPlayer :: Scene -> Scene
sControlPlayer scene = case isfireball of
    Nothing ->
        Scene
            newp
            (sFireball scene)
            (sEnemy scene)
            (sTilemap scene)
            (sControl scene)
            (sDelta scene)
    Just fb ->
        Scene
            newp
            (fb : (sFireball scene))
            (sEnemy scene)
            (sTilemap scene)
            (sControl scene)
            (sDelta scene)
  where
    (newp, isfireball) = controlPlayer (sPlayer scene)
                                       (sTilemap scene)
                                       (sDelta scene)
                                       (sControl scene)

------------------------FIREBALL_FUNCTIONS----------------------------------

--move fireballs
stepFireballs :: [Fireball] -> Tilemap -> Float -> ([Fireball], Tilemap)
stepFireballs [] tmap _         = ([], tmap)
stepFireballs (f:fs) tmap delta = case newf of
    Nothing       -> (newfs, retmap)
    Just fireball -> (fireball : newfs, retmap)
  where
    (newf, newmap)  = stepFireball f tmap delta
    (newfs, retmap) = stepFireballs fs newmap delta

--move fireball
stepFireball :: Fireball -> Tilemap -> Float -> (Maybe Fireball, Tilemap)
stepFireball f tmap delta = case cond of
    Free         -> (Just (Fireball
                              (newx, newy)
                              a
                              (fDamage f)
                              (fRadius f)
                              s
                              (fModel f))
                  , tmap)
    Blocked      -> (Nothing, tmap)
    Destructible -> (Nothing, removeDO tmap newcoord)
  where
    (x,y)    = fPos f
    a        = fRadian f
    s        = fSpeed f
    newx     = x + (delta * s * cos a)
    newy     = y - (delta * s * sin a)
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

--check fireballs for hurting enemies
damageFireballs :: [Fireball] -> [Enemy] -> ([Fireball], [Enemy])
damageFireballs [] e     = ([], e)
damageFireballs (f:fs) e = case newf of
    Nothing       -> (retlf, enret)
    Just fireball -> (fireball : retlf, enret)
  where
    (newf, newe)   = damageFireball f e
    (retlf, enret) = damageFireballs fs newe

--check fireball for hurting enemies
damageFireball :: Fireball -> [Enemy] -> (Maybe Fireball, [Enemy])
damageFireball f []           = (Just f, [])
damageFireball f (e:es)
    | rx < r, ry < r, ehp > 0 = (Nothing --hit
                              , Enemy
                                    (ex, ey)
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
    (fx,fy)       = fPos f
    r             = fRadius f
    fd            = fDamage f
    (ex,ey)       = ePos e
    ehp           = eHp e - fd
    rx            = abs (fx - ex)
    ry            = abs (fy - ey)
    (newf, enret) = damageFireball f es

-------------------------------ENEMY_FUNCTIONS------------------------------

--fuck zero division
myCos :: Float -> Float -> Float
myCos _ 0 = 1
myCos a b = a/b

--need to move closer?
moveEnemy :: Player -> Enemy -> Tilemap -> Float -> Enemy
moveEnemy p e tmap delta
    | (isPInRange p e) = e
    | otherwise        = moveEnemy2 p e tmap delta

--moves Enemy to Player
moveEnemy2 :: Player -> Enemy -> Tilemap -> Float -> Enemy
moveEnemy2 p e tmap delta = case cond of
    Free -> Enemy
                (newx, newy)
                (eHp e)
                (eDamage e)
                (eRange e)
                (eSpeed e)
                (eASpeed e)
                (eModel e)
                (eTex e)
                (eVision e)
                (eAgro e)
    _    -> e
  where
    (px, py) = pPos p
    (ex, ey) = ePos e
    es       = eSpeed e
    rx       = (px - ex)
    ry       = (py - ey)
    cosalpha = myCos rx (sqrt ((rx * rx) + (ry * ry))) --fuck zero division
    sinalpha = (sqrt (1 - (cosalpha * cosalpha))) * signum ry
    newx     = ex + (delta * es * cosalpha)
    newy     = ey + (delta * es * sinalpha)
    newcoord = (floor newy, floor newx)
    cond     = specCellCond tmap newcoord

--Enemy deal Damage
damageEnemy :: Player -> Enemy -> Float -> (Player, Enemy)
damageEnemy p e delta
    | isrange, isaready =
        (Player
            (pPos p)
            (pRadian p)
            newhp --get damaged
            (pSpeed p)
            (pASpeed p)
            (pDamage p)
      , Enemy
            (ePos e)
            (eHp e)
            (eDamage e)
            (eRange e)
            (eSpeed e)
            (Just cd, cd) --set attack cd
            (eModel e)
            (eTex e)
            (eVision e)
            (eAgro e))
    | otherwise =
        (p
      , Enemy
            (ePos e)
            (eHp e)
            (eDamage e)
            (eRange e)
            (eSpeed e)
            (delay, cd) --change attack delay
            (eModel e)
            (eTex e)
            (eVision e)
            (eAgro e))
  where
    isrange   = isPInRange p e
    newhp     = (pHp p) - (eDamage e)
    (tmp, cd) = eASpeed e
    delay     = case tmp of
                Nothing   -> Nothing
                Just time -> Just (time - delta)
    isaready  = case delay of
                Nothing   -> True
                Just time -> if (time < 0) then True else False

--is player in vision
isPInVision :: Player -> Enemy -> Bool
isPInVision p e = result
  where
    (px,py) = pPos p
    (ex,ey) = ePos e
    ev      = eVision e
    rx      = abs (px - ex)
    ry      = abs (py - ey)
    result  = (rx < ev) && (ry < ev)

--is player in range
isPInRange :: Player -> Enemy -> Bool
isPInRange p e = result
  where
    (px,py) = pPos p
    (ex,ey) = ePos e
    er     = eRange e
    rx     = abs (px - ex)
    ry     = abs (py - ey)
    result = (rx < er) && (ry < er)

--Enemy Perfet(NO) AI
--action if Agro or in vision(set Agro)
stepEnemy :: Player -> Enemy -> Tilemap -> Float -> (Player, Enemy)
stepEnemy p e tmap delta
    | agro      = damageEnemy p (moveEnemy p e tmap delta) delta
    | isvision  = damageEnemy p (moveEnemy
                                    p
                                    (Enemy
                                        (ePos e)
                                        (eHp e)
                                        (eDamage e)
                                        (eRange e)
                                        (eSpeed e)
                                        (eASpeed e)
                                        (eModel e)
                                        (eTex e)
                                        (eVision e)
                                        True) --set agro
                                    tmap
                                    delta)
                                    delta
    | otherwise = (p, e)
  where
    isvision = isPInVision p e
    agro     = eAgro e

--Enemies Perfect(NO) AI
stepEnemies :: Player -> [Enemy] -> Tilemap -> Float -> (Player, [Enemy])
stepEnemies p [] _ _            = (p, [])
stepEnemies p (e:es) tmap delta = (retp, newe : rete)
  where
  (newp, newe) = stepEnemy p e tmap delta
  (retp, rete) = stepEnemies newp es tmap delta

-----------------------------PLAYER_FUNCTIONS-------------------------------

--implements player control
controlPlayer ::
  Player
  -> Tilemap
  -> Float
  -> Control
  -> (Player, Maybe Fireball)
controlPlayer p tmap delta control
    | isattack    = case cond of
                      Free -> (Player
                                  (newx, newy)
                                  newa
                                  (pHp p)
                                  ps
                                  (Just cd, cd)
                                  pd
                            , Just (createFireball
                                  (newx, newy)
                                  newa
                                  pd))
                      _    -> (Player
                                  (px, py)
                                  newa
                                  (pHp p)
                                  ps
                                  (Just cd, cd)
                                  pd
                            , Just (createFireball
                                  (px, py)
                                  newa
                                  pd))
    | otherwise   = case cond of
                      Free -> (Player
                                  (newx, newy)
                                  newa
                                  (pHp p)
                                  ps
                                  (delay, cd)
                                  pd
                            , Nothing)
                      _    -> (Player
                                  (px, py)
                                  newa
                                  (pHp p)
                                  ps
                                  (delay, cd)
                                  pd
                            , Nothing)
  where
    (px, py)  = pPos p
    pd        = pDamage p
    pa        = pRadian p
    ps        = pSpeed p
    (tmp, cd) = pASpeed p
    delay     = case tmp of
                Nothing   -> Nothing
                Just time -> Just (time - delta)
    isforward = case (cForward control) of
                False -> 0
                True  -> 1
    isback    = case (cBack control) of
                False -> 0
                True  -> 1
    isleft    = case (cLeft control) of
                False -> 0
                True  -> 1
    isright   = case (cRight control) of
                False -> 0
                True  -> 1
    isaready  = case delay of
                Nothing   -> True
                Just time -> if (time < 0) then True else False
    isattack  = (cSpace control) && isaready
    step      = isforward - isback
    turn      = isleft - isright
    newx      = px + (step * delta * ps * cos pa)
    newy      = py - (step * delta * ps * sin pa)
    newcoord  = (floor newy, floor newx)
    cond      = specCellCond tmap newcoord
    newa      = pa + (1.2 * delta * turn)
