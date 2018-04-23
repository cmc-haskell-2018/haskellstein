module Haskellstein.Interactions where

import Prelude
import Haskellstein.Data
import Haskellstein.CheckMap
import Haskellstein.Initialize
import Haskellstein.Texconsts
import Haskellstein.Texwork

constPiF :: Float
constPiF = 3.141592

-------------------------SHELL_FUNCTIONS------------------------------------

--all interactions
doInteractions :: Scene -> Scene
doInteractions = doTexture . doEnemies . doFireballs . doPlayer

--all textures (in future)
doTexture :: Scene -> Scene
doTexture = sTexture

--shell - change textures only "b" objects now
--Checkmap function
sTexture :: Scene -> Scene
sTexture scene =
    scene {sTextureCond = newTc, sTilemap = newTmap}
  where
    (newTmap, newTc) = changeTextures (sTilemap scene)
                                      (sTextureCond scene)
                                      (sDelta scene)

--all actions of fireballs
doFireballs :: Scene -> Scene
doFireballs = sAnimationFireballs . sMoveFireballs . sDamageFireballs

--shell
sAnimationFireballs :: Scene -> Scene
sAnimationFireballs scene = scene {sFireball = retF, sEnemyFireball = retEF}
  where
    retF  = animationFireballs (sFireball scene)
                               (sDelta scene)
    retEF = animationFireballs (sEnemyFireball scene)
                               (sDelta scene)

--shell
sMoveFireballs :: Scene -> Scene
sMoveFireballs scene =
    scene {sFireball = newF, sTilemap = retTmap, sEnemyFireball = newEF}
  where
    (newF, newTmap)  = moveFireballs (sFireball scene)
                                     (sTilemap scene)
                                     (sDelta scene)
    (newEF, retTmap) = moveFireballs (sEnemyFireball scene)
                                     newTmap
                                     (sDelta scene)

--shell
sDamageFireballs :: Scene -> Scene
sDamageFireballs scene =
    scene { sFireball = newF, sEnemy = newE, sPlayer = newP
          , sEnemyFireball = newEF}
  where
    (newF, newE)  = damageFireballs (sFireball scene)
                                    (sEnemy scene)
    (newEF, newP) = damageEnemyFireballs (sEnemyFireball scene)
                                         (sPlayer scene)

--all actions of enemies
doEnemies :: Scene -> Scene
doEnemies = sDamageEnemies
          . sMoveEnemies
          . sChangeTexEnemies
          . sSetAgroEnemies
          . sExtractDeadEnemies

--shell
sSetAgroEnemies :: Scene -> Scene
sSetAgroEnemies scene =
    scene {sEnemy = newE ++ agroE}
  where
    agroE  = extractAgroEnemies (sEnemy scene)
    sleepE = extractSleepEnemies (sEnemy scene)
    newE   = setAgroEnemies agroE sleepE

--shell
sMoveEnemies :: Scene -> Scene
sMoveEnemies scene =
    scene {sEnemy = newE}
  where
    newE = moveEnemies (sPlayer scene)
                       (sEnemy scene)
                       (sTilemap scene)
                       (sDelta scene)

--shell
sDamageEnemies :: Scene -> Scene
sDamageEnemies scene =
    scene { sPlayer        = newP
          , sEnemy         = newE
          , sEnemyFireball = newEF ++ (sEnemyFireball scene)}
  where
    (newP, newE, newEF) = damageEnemies (sPlayer scene)
                                        (sEnemy scene)
                                        (sDelta scene)

--shell
sChangeTexEnemies :: Scene -> Scene
sChangeTexEnemies scene =
    scene {sEnemy = newE, sDeadEnemy = newDE}
  where
    newE  = changeTexEnemies (sEnemy scene)
                             (sDelta scene)
    newDE = changeTexEnemies (sDeadEnemy scene)
                             (sDelta scene)

--shell
sExtractDeadEnemies :: Scene -> Scene
sExtractDeadEnemies scene =
    scene {sEnemy = newE, sDeadEnemy = (newDE ++ oldDE)}
  where
    oldDE         = (sDeadEnemy scene)
    (newE, newDE) = extractDeadEnemies (sEnemy scene)

--all actions of player
doPlayer :: Scene -> Scene
doPlayer = sItemsPlayer . sExitPlayer . sCastPlayer . sMovePlayer

--shell
sMovePlayer :: Scene -> Scene
sMovePlayer scene =
    scene {sPlayer = newP}
  where
    newP = movePlayer (sPlayer scene)
                      (sTilemap scene)
                      (sDelta scene)
                      (sControl scene)

--shell
sCastPlayer :: Scene -> Scene
sCastPlayer scene = case isFireball of
    Nothing ->
        scene {sPlayer = newP}
    Just fb ->
        scene {sPlayer = newP, sFireball = (fb : (sFireball scene))}
  where
    (newP, isFireball) = castPlayer (sPlayer scene)
                                    (sDelta scene)
                                    (sControl scene)

--shell
sExitPlayer :: Scene -> Scene
sExitPlayer scene = scene {sPlayer = exitPlayer (sPlayer scene)
                                                (sTilemap scene)}

--shell
sItemsPlayer :: Scene -> Scene
sItemsPlayer scene = scene {sPlayer = newP, sTilemap = newTmap}
  where
    (newP, newTmap) = itemsPlayer (sPlayer scene)
                                  (sTilemap scene)

------------------------FIREBALL_FUNCTIONS----------------------------------

--move fireballs
moveFireballs :: [Fireball] -> Tilemap -> Float -> ([Fireball], Tilemap)
moveFireballs [] tmap _         = ([], tmap)
moveFireballs (f:fs) tmap delta = case newF of
    Nothing       -> (newFs, retTmap)
    Just fireball -> (fireball : newFs, retTmap)
  where
    (newF, newTmap)  = moveFireball f tmap delta
    (newFs, retTmap) = moveFireballs fs newTmap delta

--move fireball
moveFireball :: Fireball -> Tilemap -> Float -> (Maybe Fireball, Tilemap)
moveFireball f tmap delta = case cond of
    Free         -> (Just (f {fPos = (newX, newY)}), tmap)
    Blocked      -> (Nothing, tmap)
    Destructible -> (Nothing, removeDO tmap newCoord)
  where
    (x,y)    = fPos f
    a        = fRadian f
    s        = fSpeed f
    newX     = x + (delta * s * cos a)
    newY     = y + (delta * s * sin a)
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
    | rx < r, ry < r = (Nothing --hit
                     , e {eHp = ehp, eAgro = True} : es)
    | otherwise      = (newF, e : retE) --miss
  where
    (fx,fy)       = fPos f
    r             = fRadius f
    fd            = fDamage f
    (ex,ey)       = ePos e
    ehp           = eHp e - fd
    rx            = abs (fx - ex)
    ry            = abs (fy - ey)
    (newF, retE)  = damageFireball f es

--check fireballs for hurting player
damageEnemyFireballs :: [Fireball] -> Player -> ([Fireball], Player)
damageEnemyFireballs [] p     = ([], p)
damageEnemyFireballs (f:fs) p = case newF of
    Nothing       -> (retLF, retP)
    Just fireball -> (fireball : retLF, retP)
  where
    (newF, newP)   = damageEnemyFireball f p
    (retLF, retP)  = damageEnemyFireballs fs newP

--check fireball for hurting player
damageEnemyFireball :: Fireball -> Player -> (Maybe Fireball, Player)
damageEnemyFireball f p
    | rx < r, ry < r = (Nothing --hit
                     , p {pHp = php})
    | otherwise      = (Just f, p) --miss
  where
    (fx,fy)       = fPos f
    r             = fRadius f
    fd            = fDamage f
    (px,py)       = pPos p
    php           = pHp p - fd
    rx            = abs (fx - px)
    ry            = abs (fy - py)

--Animation of fireballs
animationFireballs :: [Fireball] -> Float -> [Fireball]
animationFireballs [] _         = []
animationFireballs (f:fs) delta = newF : retF
  where
    newF = animationFireball f delta
    retF = animationFireballs fs delta

--Animation of fireball
animationFireball :: Fireball -> Float -> Fireball
animationFireball f delta
    | delay == Nothing = f {fAnim = (Just cd, cd), fTex = (swapFT $ fTex f)}
    | otherwise        = f {fAnim = (delay, cd)}
  where
    (tmp, cd) = fAnim f
    delay     = case tmp of
                Nothing   -> Nothing
                Just time -> if (time - delta < 0) then Nothing
                             else Just (time - delta)

--swapFireballTexture
swapFT :: ObjectTexture -> ObjectTexture
swapFT tex
    | tex == fireballTex1 = fireballTex2
    | tex == fireballTex2 = fireballTex1
    | tex == fireballTex3 = fireballTex4
    | tex == fireballTex4 = fireballTex3
    | tex == fireballTex5 = fireballTex6
    | tex == fireballTex6 = fireballTex5
    | otherwise           = fireballTex1

-------------------------------ENEMY_FUNCTIONS------------------------------

--if enemy is range
ifRangeType :: EnemyType -> Bool
ifRangeType t
  | t == Range = True
  | t == Mage  = True
  | otherwise  = False

--change enemies texture
changeTexEnemies :: [Enemy] -> Float -> [Enemy]
changeTexEnemies [] _         = []
changeTexEnemies (e:es) delta = newE : retE
  where
    newE = changeTexEnemy e delta
    retE = changeTexEnemies es delta

-- change enemy texture
changeTexEnemy :: Enemy -> Float -> Enemy
changeTexEnemy e delta
    | delay == Nothing = e {eAnim = (Just cd, cd), eTex = (swapET (eTex e))}
    | otherwise        = e {eAnim = (delay, cd)}
  where
    (tmp, cd) = eAnim e
    delayTmp  = case tmp of
                Nothing   -> Nothing
                Just time -> if (time - delta < 0) then Nothing
                             else Just (time - delta)
    delay     = if (eAgro e) && ((eMoved e)
                                 || checkAttackTex (eTex e)
                                 || checkDeathTex (eTex e))
                  then delayTmp
                  else tmp

--split dead and alive enemies
extractDeadEnemies :: [Enemy] -> ([Enemy], [Enemy])
extractDeadEnemies [] = ([], [])
extractDeadEnemies (e:es)
    | isDead    = (newE, e {eTex = newTex, eAnim = (Just cd, cd)} : newDE)
    | otherwise = (e : newE, newDE)
  where
    newTex        = setDT (eTex e)
    (_, cd)       = eAnim e
    isDead        = (eHp e) <= 0
    (newE, newDE) = extractDeadEnemies es

--take Agro enemies
extractAgroEnemies :: [Enemy] -> [Enemy]
extractAgroEnemies []     = []
extractAgroEnemies (e:es) = if (eAgro e)
                            then (e : extractAgroEnemies es)
                            else (extractAgroEnemies es)

--take sleep enemies
extractSleepEnemies :: [Enemy] -> [Enemy]
extractSleepEnemies []     = []
extractSleepEnemies (e:es) = if (eAgro e)
                            then (extractSleepEnemies es)
                            else (e : extractSleepEnemies es)

--agro enemies triger agro of sleep enemies
setAgroEnemies :: [Enemy] -> [Enemy] -> [Enemy]
setAgroEnemies [] sleepE     = sleepE
setAgroEnemies (e:es) sleepE = retSleepE
  where
    newSleepE = setAgroEnemy e sleepE
    retSleepE = setAgroEnemies es newSleepE

--agro enemy triger agro of sleep enemies
setAgroEnemy :: Enemy -> [Enemy] -> [Enemy]
setAgroEnemy _ []       = []
setAgroEnemy e (se:ses)
    | ifSet     = se {eAgro = True} : (setAgroEnemy e ses)
    | otherwise = se : (setAgroEnemy e ses)
  where
    (ex, ey)   = ePos e
    (sex, sey) = ePos se
    rx         = abs (ex - sex)
    ry         = abs (ey - sey)
    ifSet      = rx < eVision se && ry < eVision se

--dont zero devide
myCos :: Float -> Float -> Float
myCos _ 0 = 1
myCos a b = a/b

--find angle to player
findAngle :: Position -> Position -> Float
findAngle (px,py) (ex,ey) = a
  where
    rx       = (px - ex)
    ry       = (py - ey)
    cosAlpha = myCos rx (sqrt ((rx * rx) + (ry * ry)))
    half     = if ((signum ry) == 0)
               then 1
               else (signum ry)
    a        = (acos cosAlpha) * half

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
    er      = eRange e
    rx      = abs (px - ex)
    ry      = abs (py - ey)
    result  = (rx < er) && (ry < er)

--enemies movement
moveEnemies :: Player -> [Enemy] -> Tilemap -> Float -> [Enemy]
moveEnemies _ [] _ _            = []
moveEnemies p (e:es) tmap delta = retE : restE
  where
    retE  = moveEnemy p e tmap delta
    restE = moveEnemies p es tmap delta

--moveCheck
moveEnemy :: Player -> Enemy -> Tilemap -> Float -> Enemy
moveEnemy p e tmap delta
    | isRange, agro   = retE --already near
    | agro            = moveEnemy2 p retE tmap delta
    | otherwise       = retE
  where
    isRange  = isPInRange p e
    isVision = isPInVision p e
    retE     = if isVision then e {eAgro = True, eMoved = False}
               else e {eMoved = False}
    agro     = eAgro retE

--moves Enemy to Player
moveEnemy2 :: Player -> Enemy -> Tilemap -> Float -> Enemy
moveEnemy2 p e tmap delta =
    e {ePos = newCoord, eMoved = True}
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
    newCoord = getNewCoord (ex, ey) (newX, newY) tmap

--enemies attack phase
damageEnemies :: Player -> [Enemy] -> Float -> (Player, [Enemy], [Fireball])
damageEnemies p [] _         = (p, [], [])
damageEnemies p (e:es) delta = (retP, newE : retE, newF ++ retF)
  where
    (newP, newE, newF) = if (ifRangeType $ eModel e)
                         then damageEnemyRange p e delta
                         else damageEnemy p e delta
    (retP, retE, retF) = damageEnemies newP es delta

--Enemy deal Damage
damageEnemy :: Player -> Enemy -> Float -> (Player, Enemy, [Fireball])
damageEnemy p e delta
    | isRange, isAReady, agro = (p {pHp = newHp}
                              , e {eASpeed = (Just cd, cd)
                                , eAnim = (Just (cdA), cdA)
                                , eTex = setAT (eTex e)}
                              , [])
    | otherwise               = (p, e {eASpeed = (delay, cd)}, [])
  where
    (_, cdA)  = eAnim e
    isRange   = isPInRange p e
    agro      = eAgro e
    newHp     = (pHp p) - (eDamage e)
    (tmp, cd) = eASpeed e
    delay     = case tmp of
                Nothing   -> Nothing
                Just time -> if (time - delta < 0) then Nothing
                             else Just (time - delta)
    isAReady  = case delay of
                Nothing -> True
                _       -> False

--Range enemy cast
damageEnemyRange :: Player -> Enemy -> Float -> (Player, Enemy, [Fireball])
damageEnemyRange p e delta
    | isRange, isAReady, agro =
        (p
        , e {eASpeed = (Just cd, cd)
          , eAnim = (Just (cdA), cdA)
          , eTex = setAT (eTex e)}
        , [createFireballEnemy pos a d model]
       ++ [createFireballEnemy pos (a + (constPiF / 10)) d model]
       ++ [createFireballEnemy pos (a - (constPiF / 10)) d model])
    | otherwise               = (p, e {eASpeed = (delay, cd)}, [])
  where
    (_, cdA)  = eAnim e
    isRange   = isPInRange p e
    agro      = eAgro e
    (tmp, cd) = eASpeed e
    d         = eDamage e
    pos       = ePos e
    a         = findAngle (pPos p) pos
    model     = eModel e
    delay     = case tmp of
                Nothing   -> Nothing
                Just time -> if (time - delta < 0) then Nothing
                             else Just (time - delta)
    isAReady  = case delay of
                Nothing -> True
                _       -> False

-----------------------------PLAYER_FUNCTIONS-------------------------------

--whereToMove
pMoveDir :: Control -> Float
pMoveDir control = isForward - isBack
  where
    isForward = case (cForward control) of
                False -> 0
                True  -> 1
    isBack    = case (cBack control) of
                False -> 0
                True  -> 1

--whereToMoveH
pMoveDirH :: Control -> Float
pMoveDirH control = isLeft - isRight
  where
    isLeft  = case (cLeftM control) of
                False -> 0
                True  -> 1
    isRight = case (cRightM control) of
                False -> 0
                True  -> 1

--whereToTurn
pTurnDir :: Control -> Float
pTurnDir control = isRight - isLeft
  where
    isLeft  = case (cLeftT control) of
                False -> 0
                True  -> 1
    isRight = case (cRightT control) of
                False -> 0
                True  -> 1

--changePlayerPos
movePlayer
  :: Player
  -> Tilemap
  -> Float
  -> Control
  -> Player
movePlayer p tm delta control
  | pTurnAround p == Nothing = movePlayerControl p tm delta control
  | otherwise                = movePlayerTurn p delta

--changePosByControl
movePlayerControl
  :: Player
  -> Tilemap
  -> Float
  -> Control
  -> Player
movePlayerControl p tmap delta control =
    p {pPos = newCoord, pRadian = newA, pTurnAround = isTurnA}
  where
    (px, py) = pPos p
    pa       = pRadian p
    ps       = pSpeed p
    isTurnA  = case (cTurnAround control) of
               False -> Nothing
               True  -> Just constPiF
    step     = pMoveDir control
    stepH    = pMoveDirH control
    turn     = pTurnDir control
    tmpX     = px + (step * delta * ps * cos pa)
                  + (stepH * delta * ps * cos (pa - constPiF / 2))
    tmpY     = py + (step * delta * ps * sin pa)
                  + (stepH * delta * ps * sin (pa - constPiF / 2))
    newCoord = getNewCoord (px, py) (tmpX, tmpY) tmap
    newA     = pa + (constPiF / 2 * delta * turn)

--changePosByAnimation
movePlayerTurn
  :: Player
  -> Float
  -> Player
movePlayerTurn p delta =
    p {pRadian = newA, pTurnAround = resA (pTurnAround p)}
  where
    turnTime        = 0.3
    turnA           = delta * constPiF / turnTime
    newA            = (pRadian p) - turnA
    resA Nothing    = Nothing -- unreal case, made to avoid warnings
    resA (Just val) = case ((val - turnA) <= 0) of  
                          True  -> Nothing
                          False -> Just (val - turnA)

--playerSpellCast
castPlayer
  :: Player
  -> Float
  -> Control
  -> (Player, Maybe Fireball)
castPlayer p delta control
    | isAttack    = (p {pASpeed = (Just cd, cd)}
                  , Just (createFireball
                             (px, py)
                             pa
                             pd
                             (pFType p)))
    | otherwise   = (p {pASpeed = (delay, cd)}, Nothing)
  where
    (px, py)  = pPos p
    pd        = pDamage p
    pa        = pRadian p
    (tmp, cd) = pASpeed p
    delay     = case tmp of
                Nothing   -> Nothing
                Just time -> if (time - delta < 0) then Nothing
                             else Just (time - delta)
    isAReady  = case delay of
                Nothing -> True
                _       -> False
    isAttack  = (cSpace control) && isAReady

--set exit level status
exitPlayer :: Player -> Tilemap -> Player
exitPlayer p tmap = p {pExit = status}
  where
    (x, y) = pPos p
    status = exitCell tmap (floor y, floor x)

--take items
itemsPlayer :: Player -> Tilemap -> (Player, Tilemap)
itemsPlayer p tmap
    | potion                = (p {pHp = (pHp p) + 10}, newPTmap)
    | elec && not potion    = (p {pElec = True}, newETmap)
    | otherwise             = (p, tmap)
  where
    (x, y)             = pPos p
    (newPTmap, potion) = potionCell tmap (floor y, floor x)
    (newETmap, elec)   = elecCell tmap (floor y, floor x)
