module Initialize where

import Prelude
import Data

createPlayer :: Float -> Float -> Player
createPlayer x y = Player x -- pPosX
                          y -- pPosY
                          0 -- pRadian
                          7 -- pHp
                          7 -- pSpeed
                          0 -- pBuff

createEnemy :: Float -> Float -> Enemy
createEnemy x y = Enemy x -- ePosX
                        y -- ePosY
                        7 -- eHp
                        7 -- eDamage
                        7 -- eRange
                        7 -- eSpeed
                        7 -- eModel
                        7 -- eTex
                        7 -- eVision
                        False -- eAgro

createFireball :: Float -> Float -> Float -> Fireball
createFireball x y a = Fireball x -- fPosX
                                y -- fPosY
                                a -- fRadian
                                7 -- fDamage
                                7 -- fRadius
                                7 -- fSpeed
                                7 -- fModel

--split string by symbol
splitString :: Char -> String -> (String, String)
splitString _ [] = ([], [])
splitString c (x:xs)
    | x == c    = ([], xs)
    | otherwise = (x : fst ret, snd ret)
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
findPlayer2 :: Float -> Tilemap -> Player
findPlayer2 _ [] = createPlayer 0 0
findPlayer2 y (str:rest)
    | findP == len = findPlayer2 (y + 1) rest
    | otherwise    = createPlayer ((fromIntegral findP) + 0.5) (y + 0.5)
      where
        len   = length str
        findP = len - (length (dropWhile (\x -> head x /= 'p') str))

--create enemies from tilemap positions
findEnemies :: Tilemap -> [Enemy]
findEnemies = findEnemies2 0

--help function
findEnemies2 :: Float -> Tilemap -> [Enemy]
findEnemies2 _ [] = []
findEnemies2 y (str:rest)
    | findE == len = findEnemies2 (y + 1) rest
    | otherwise    = createEnemy ((fromIntegral findE) + 0.5) (y + 0.5) :
                     findEnemies2 y ((sl ++ ["v00"] ++ (tail sr)) : rest)
      where
        len   = length str
        sr  = dropWhile (\x -> head x /= 'e') str
        sl  = takeWhile (\x -> head x /= 'e') str
        findE = len - (length sr)

createScene :: Tilemap -> Scene
createScene tmap = (findPlayer tmap, [], findEnemies tmap, edit)
  where
    rewrite p
        | head p == 'e' = "v00"
        | head p == 'p' = "v00"
        | otherwise     = p
    edit = map (map (rewrite)) tmap
