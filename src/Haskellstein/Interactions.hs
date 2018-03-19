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
  where
    (newf, newtmap) = stepFireballs (sFireball scene) (sTilemap scene)

sDamageFireballs :: Scene -> Scene
sDamageFireballs scene =
    Scene
        (sPlayer scene)
        newf
        newe
        (sTilemap scene)
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
  where
    (newp, newe) = stepEnemies (sPlayer scene)
                               (sEnemy scene)
                               (sTilemap scene)

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
    Just fb ->
        Scene
            newp
            (fb : (sFireball scene))
            (sEnemy scene)
            (sTilemap scene)
  where
    (newp, isfireball) = controlPlayer (sPlayer scene)
                                       (sTilemap scene)

------------------------FIREBALL_FUNCTIONS----------------------------------

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
    delta    = 0.01 --need to be timer dif
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
moveEnemy :: Player -> Enemy -> Tilemap -> Enemy
moveEnemy p e tmap
    | (isPInRange p e) = e
    | otherwise        = moveEnemy2 p e tmap

--moves Enemy to Player
moveEnemy2 :: Player -> Enemy -> Tilemap -> Enemy
moveEnemy2 p e tmap = case cond of
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
    delta    = 0.01 --need to be timer dif
    cosalpha = myCos rx (sqrt ((rx * rx) + (ry * ry))) --fuck zero division
    sinalpha = (sqrt (1 - (cosalpha * cosalpha))) * signum ry
    newx     = ex + (delta * es * cosalpha)
    newy     = ey + (delta * es * sinalpha)
    newcoord = (floor newy, floor newx)
    cond     = specCellCond tmap newcoord

--Enemy deal Damage
damageEnemy :: Player -> Enemy -> (Player, Enemy)
damageEnemy p e
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
    delta     = 0.01 --need to be timer diff
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
stepEnemy :: Player -> Enemy -> Tilemap -> (Player, Enemy)
stepEnemy p e tmap
    | agro      = damageEnemy p (moveEnemy p e tmap)
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
                                    tmap)
    | otherwise = (p, e)
  where
    isvision = isPInVision p e
    agro     = eAgro e

--Enemies Perfect(NO) AI
stepEnemies :: Player -> [Enemy] -> Tilemap -> (Player, [Enemy])
stepEnemies p [] _        = (p, [])
stepEnemies p (e:es) tmap = (retp, newe : rete)
  where
  (newp, newe) = stepEnemy p e tmap
  (retp, rete) = stepEnemies newp es tmap

-----------------------------PLAYER_FUNCTIONS-------------------------------

--implements player control
controlPlayer :: Player -> Tilemap -> (Player, Maybe Fireball)
controlPlayer p tmap
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
    delta     = 0.01 --need timer
    delay     = case tmp of
                Nothing   -> Nothing
                Just time -> Just (time - delta)
    isforward = 1 --pressed 'w'
    isback    = 0 --pressed 's'
    isleft    = 0 --pressed 'a'
    isright   = 0 --pressed 'd'
    isspace   = 1 --pressed 'space'
    isaready  = case delay of
                Nothing   -> True
                Just time -> if (time < 0) then True else False
    isattack  = (isspace == 1) && isaready
    step      = isforward - isback
    turn      = isleft - isright
    newx      = px + (step * delta * ps * cos pa)
    newy      = py - (step * delta * ps * sin pa)
    newcoord  = (floor newy, floor newx)
    cond      = specCellCond tmap newcoord
    newa      = pa + (0.785 * delta * turn) --turn by pi/4 in one second
