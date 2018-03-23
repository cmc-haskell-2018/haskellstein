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
        newF
        (sEnemy scene)
        newTmap
        (sControl scene)
        (sDelta scene)
  where
    (newF, newTmap) = stepFireballs (sFireball scene)
                                    (sTilemap scene)
                                    (sDelta scene)

sDamageFireballs :: Scene -> Scene
sDamageFireballs scene =
    Scene
        (sPlayer scene)
        newF
        newE
        (sTilemap scene)
        (sControl scene)
        (sDelta scene)
  where
    (newF, newE) = damageFireballs (sFireball scene) (sEnemy scene)

--all actions of enemies
doEnemies :: Scene -> Scene
doEnemies = sStepEnemies

sStepEnemies :: Scene -> Scene
sStepEnemies scene =
    Scene
        newP
        (sFireball scene)
        newE
        (sTilemap scene)
        (sControl scene)
        (sDelta scene)
  where
    (newP, newE) = stepEnemies (sPlayer scene)
                               (sEnemy scene)
                               (sTilemap scene)
                               (sDelta scene)

--all actions of player
doPlayer :: Scene -> Scene
doPlayer = sControlPlayer

sControlPlayer :: Scene -> Scene
sControlPlayer scene = case isFireball of
    Nothing ->
        Scene
            newP
            (sFireball scene)
            (sEnemy scene)
            (sTilemap scene)
            (sControl scene)
            (sDelta scene)
    Just fb ->
        Scene
            newP
            (fb : (sFireball scene))
            (sEnemy scene)
            (sTilemap scene)
            (sControl scene)
            (sDelta scene)
  where
    (newP, isFireball) = controlPlayer (sPlayer scene)
                                       (sTilemap scene)
                                       (sDelta scene)
                                       (sControl scene)

------------------------FIREBALL_FUNCTIONS----------------------------------

--move fireballs
stepFireballs :: [Fireball] -> Tilemap -> Float -> ([Fireball], Tilemap)
stepFireballs [] tmap _         = ([], tmap)
stepFireballs (f:fs) tmap delta = case newF of
    Nothing       -> (newFs, retTmap)
    Just fireball -> (fireball : newFs, retTmap)
  where
    (newF, newTmap)  = stepFireball f tmap delta
    (newFs, retTmap) = stepFireballs fs newTmap delta

--move fireball
stepFireball :: Fireball -> Tilemap -> Float -> (Maybe Fireball, Tilemap)
stepFireball f tmap delta = case cond of
    Free         -> (Just (Fireball
                              (newX, newY)
                              a
                              (fDamage f)
                              (fRadius f)
                              s
                              (fModel f))
                  , tmap)
    Blocked      -> (Nothing, tmap)
    Destructible -> (Nothing, removeDO tmap newCoord)
  where
    (x,y)    = fPos f
    a        = fRadian f
    s        = fSpeed f
    newX     = x + (delta * s * cos a)
    newY     = y - (delta * s * sin a)
    newCoord = (floor newY, floor newX)
    cond     = specCellCond tmap newCoord

--remove near Destructible objects
removeDO :: Tilemap -> CellCoord -> Tilemap
removeDO tmap (y, x) = newTmap
  where
    m1      = writeEmpty tmap (y, x)
    m2      = writeEmpty m1 (y, x + 1)
    m3      = writeEmpty m2 (y + 1, x)
    m4      = writeEmpty m3 (y, x - 1)
    newTmap = writeEmpty m4 (y - 1, x)

--check fireballs for hurting enemies
damageFireballs :: [Fireball] -> [Enemy] -> ([Fireball], [Enemy])
damageFireballs [] e     = ([], e)
damageFireballs (f:fs) e = case newF of
    Nothing       -> (retLF, retE)
    Just fireball -> (fireball : retLF, retE)
  where
    (newF, newE)   = damageFireball f e
    (retLF, retE)  = damageFireballs fs newE

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
    | otherwise               = (newF, e : retE) --miss
  where
    (fx,fy)       = fPos f
    r             = fRadius f
    fd            = fDamage f
    (ex,ey)       = ePos e
    ehp           = eHp e - fd
    rx            = abs (fx - ex)
    ry            = abs (fy - ey)
    (newF, retE)  = damageFireball f es

-------------------------------ENEMY_FUNCTIONS------------------------------

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
                (newX, newY)
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
    cosAlpha = myCos rx (sqrt ((rx * rx) + (ry * ry)))
    sinAlpha = (sqrt (1 - (cosAlpha * cosAlpha))) * signum ry
    newX     = ex + (delta * es * cosAlpha)
    newY     = ey + (delta * es * sinAlpha)
    newCoord = (floor newY, floor newX)
    cond     = specCellCond tmap newCoord

--Enemy deal Damage
damageEnemy :: Player -> Enemy -> Float -> (Player, Enemy)
damageEnemy p e delta
    | isRange, isAReady =
        (Player
            (pPos p)
            (pRadian p)
            newHp --get damaged
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
    isRange   = isPInRange p e
    newHp     = (pHp p) - (eDamage e)
    (tmp, cd) = eASpeed e
    delay     = case tmp of
                Nothing   -> Nothing
                Just time -> Just (time - delta)
    isAReady  = case delay of
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
    | isVision  = damageEnemy p (moveEnemy
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
    isVision = isPInVision p e
    agro     = eAgro e

--Enemies Perfect(NO) AI
stepEnemies :: Player -> [Enemy] -> Tilemap -> Float -> (Player, [Enemy])
stepEnemies p [] _ _            = (p, [])
stepEnemies p (e:es) tmap delta = (retP, newE : retE)
  where
  (newP, newE) = stepEnemy p e tmap delta
  (retP, retE) = stepEnemies newP es tmap delta

-----------------------------PLAYER_FUNCTIONS-------------------------------

--implements player control
controlPlayer
  ::Player
  -> Tilemap
  -> Float
  -> Control
  -> (Player, Maybe Fireball)
controlPlayer p tmap delta control
    | isAttack    = case cond of
                      Free -> (Player
                                  (newX, newY)
                                  newA
                                  (pHp p)
                                  ps
                                  (Just cd, cd)
                                  pd
                            , Just (createFireball
                                  (newX, newY)
                                  newA
                                  pd))
                      _    -> (Player
                                  (px, py)
                                  newA
                                  (pHp p)
                                  ps
                                  (Just cd, cd)
                                  pd
                            , Just (createFireball
                                  (px, py)
                                  newA
                                  pd))
    | otherwise   = case cond of
                      Free -> (Player
                                  (newX, newY)
                                  newA
                                  (pHp p)
                                  ps
                                  (delay, cd)
                                  pd
                            , Nothing)
                      _    -> (Player
                                  (px, py)
                                  newA
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
    isForward = case (cForward control) of
                False -> 0
                True  -> 1
    isBack    = case (cBack control) of
                False -> 0
                True  -> 1
    isLeft    = case (cLeft control) of
                False -> 0
                True  -> 1
    isRight   = case (cRight control) of
                False -> 0
                True  -> 1
    isAready  = case delay of
                Nothing   -> True
                Just time -> if (time < 0) then True else False
    isAttack  = (cSpace control) && isAready
    step      = isForward - isBack
    turn      = isLeft - isRight
    newX      = px + (step * delta * ps * cos pa)
    newY      = py - (step * delta * ps * sin pa)
    newCoord  = (floor newY, floor newX)
    cond      = specCellCond tmap newCoord
    newA      = pa + (1.2 * delta * turn)
