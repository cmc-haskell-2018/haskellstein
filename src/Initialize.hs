module Initialize where

import Prelude
import Data

createPlayer :: CellCoord -> Player
createPlayer (y, x) = Player ((fromIntegral x) + 0.5) -- pPosX
                             ((fromIntegral y) + 0.5) -- pPosY
                             0 -- pRadian
                             11 -- pHp
                             0.5 -- pSpeed
                             (1, 1) -- pASpeed
                             1 -- pDamage

createEnemy :: CellCoord -> Int -> Enemy
--melee
createEnemy (y, x) 1 = Enemy ((fromIntegral x) + 0.5) -- ePosX
                             ((fromIntegral y) + 0.5) -- ePosY
                             3 -- eHp
                             1 -- eDamage
                             1 -- eRange
                             0.5 -- eSpeed
                             (2, 2) -- eASpeed
                             1 -- eModel
                             1 -- eTex
                             8 -- eVision
                             False -- eAgro
--range
createEnemy (y, x) 2 = Enemy ((fromIntegral x) + 0.5) -- ePosX
                             ((fromIntegral y) + 0.5) -- ePosY
                             2 -- eHp
                             1 -- eDamage
                             3 -- eRange
                             0.4 -- eSpeed
                             (2, 2) -- eASpeed
                             2 -- eModel
                             1 -- eTex
                             8 -- eVision
                             False -- eAgro
--spike
createEnemy (y, x) _ = Enemy ((fromIntegral x) + 0.5) -- ePosX
                             ((fromIntegral y) + 0.5) -- ePosY
                             5 -- eHp
                             1 -- eDamage
                             1 -- eRange
                             0 -- eSpeed
                             (1, 1) -- eASpeed
                             3 -- eModel
                             1 -- eTex
                             0 -- eVision
                             False -- eAgro

createFireball :: CellCoord -> Float -> Int -> Fireball
createFireball (y, x) a d = Fireball (fromIntegral x) -- fPosX
                                     (fromIntegral y) -- fPosY
                                     a -- fRadian
                                     d -- fDamage
                                     1 -- fRadius
                                     0.7 -- fSpeed
                                     1 -- fModel

--split string by symbol
splitString :: Char -> String -> (String, String)
splitString _ [] = ([], [])
splitString c (x:xs)
    | x == c     = ([], xs)
    | otherwise  = (x : fst ret, snd ret)
      where
        ret = splitString c xs

--substrnigs from symbol
subStrings :: Char -> String -> [String]
subStrings _ []  = []
subStrings c str = first : subStrings c rest
  where
    result = splitString c str
    first  = fst result
    rest   = snd result

createTilemap :: String -> Tilemap
createTilemap str = map (subStrings ' ') $ subStrings '\n' str

--create player from tilemap position
findPlayer :: Tilemap -> Player
findPlayer = findPlayer2 0

--help function
findPlayer2 :: Int -> Tilemap -> Player
findPlayer2 _ []   = createPlayer (0, 0)
findPlayer2 y (str:rest)
    | findP == len = findPlayer2 (y + 1) rest
    | otherwise    = createPlayer (y, findP)
      where
        len   = length str
        findP = len - (length (dropWhile (\x -> head x /= 'p') str))

--create enemies from tilemap positions
findEnemies :: Tilemap -> [Enemy]
findEnemies = findEnemies2 0

--help function
findEnemies2 :: Int -> Tilemap -> [Enemy]
findEnemies2 _ []  = []
findEnemies2 y (str:rest)
    | findE == len = findEnemies2 (y + 1) rest
    | otherwise    = createEnemy (y, findE) t :
                     findEnemies2 y ((sl ++ ["v00"] ++ (tail sr)) : rest)
      where
        len   = length str
        sr    = dropWhile (\x -> head x /= 'e') str
        t     = read . tail . head $ sr
        sl    = takeWhile (\x -> head x /= 'e') str
        findE = len - (length sr)

createScene :: Tilemap -> Scene
createScene tmap = (findPlayer tmap, [], findEnemies tmap, edit)
  where
    rewrite p
        | head p == 'e' = "v00"
        | head p == 'p' = "v00"
        | otherwise     = p
    edit = map (map (rewrite)) tmap
