module Haskellstein.Map where

import Prelude hiding (Right,Left)
import Data.List (delete, elemIndices)
import Haskellstein.Data
import Haskellstein.CheckMap
import System.Random

defCountOfLevels :: Int
defCountOfLevels = 2

-- ! Approach !: Tilemap can be only square

data Direction = Left | Right | Up | Down
  deriving (Eq,Show)

-- | dont understand why Maybe doesnt contain getter function
-- obviously you should avoid Nothing input
getMaybeVal :: Maybe a -> a
getMaybeVal (Just v) = v
getMaybeVal Nothing  = undefined

defMapSize :: Int
defMapSize = 32

player :: String
player = "p00"

-- | blocked wall
bwall :: String
bwall = "w00"

-- | destructible wall
dwall :: String
dwall = "d02"

gap :: String
gap = "v00"

buffs :: [String]
buffs = ["i04","i08","i09"]

portal :: String
portal = "b00"

typeEnemy :: Int -> String
typeEnemy e = "e" ++ show e

printTileMap :: Tilemap -> IO ()
printTileMap = mapM_ print

printTileMap':: Tilemap -> IO ()
printTileMap' tm = mapM_ (putStrLn . doubleChar . concat) tm
  where
    doubleChar = concatMap (\c -> ' ' : c : " ")

-- | view: (example with size == 4)
-- w00 w00 w00 w00
-- w00 v00 v00 w00
-- w00 v00 v00 w00
-- w00 w00 w00 w00
startMap :: Int -> Tilemap
startMap size = modifyAtRectangle (\_ -> gap) (1,1) (size-2,size-2) s
  where
    s = replicate size $ replicate size bwall

-- | take random elements from list with different values
takeRandom :: (RandomGen g, Eq a) => g -> Int -> [a] -> [a]
takeRandom _ _ [] = []
takeRandom g count l =
  if count == 0
  then []
  else val : takeRandom g' (count-1) l
  where
    (i,g') = randomR (0, length l - 1) g
    val = l !! i

freeAround :: Tilemap -> CellCoord -> Bool
freeAround tm (y,x) = not $ any (== False)
     [takeCellStr tm (h,w) == gap | h <- [y-1 .. y+1], w <- [x-1 .. x+1]]

-- | square 3x3 around given cell without it
aroundPlace :: CellCoord -> [CellCoord]
aroundPlace (y,x) =
     delete (y,x) [(h,w) | h <- [y-1 .. y+1], w <- [x-1 .. x+1]]


-- | i - counter, which decrease with every new check
-- simple mechanism to avoid infinite loop
genStartCoord :: (RandomGen g) => g -> Tilemap ->
                  (Maybe CellCoord, g)
genStartCoord gen tm =
  if squareGaps /= []
  then (Just coord, g1)
  else (Nothing, g1)
  where
    squareGaps = gapSquarePlaces tm
    (i,g1) = randomR (0, length squareGaps - 1) gen
    coord = squareGaps !! i

--------------------WALLS---------------------------------------------------

-- | algorithm of building chains of bwalls
iterBuildWalls :: (RandomGen g) => g -> Int ->
                  Tilemap -> Tilemap
iterBuildWalls gen chainCount tm =
  if chainCount == 0 || mCoord == Nothing
  then tm
  else iterBuildWalls g' (chainCount - 1) newMap
  where
    (mCoord,g) = genStartCoord gen tm
    coord = getMaybeVal mCoord
    g' = snd $ next g
    startStepWalls = writeCondition tm coord bwall
    newMap = buildWalls g coord startStepWalls

buildWalls :: (RandomGen g) => g -> CellCoord ->
              Tilemap -> Tilemap
buildWalls gen fstCoord tmap = stepWalls gen fstCoord tmap
  where
    stepWalls :: (RandomGen g) => g -> CellCoord ->
                Tilemap -> Tilemap
    stepWalls g (y,x) tm =
      if way /= []
      then stepWalls g'' step $ writeCondition tm step bwall
      else tm
      where
        variations = [(y,x-1),(y,x+1),(y-1,x),(y+1,x)]
        way = [(n,m) | (n,m) <- variations,
                (tm !! n) !! m /= bwall && closerWall (n,m)]
        step = way !! i
        (i,g'') = randomR (0, length way - 1) g
        dir st
          | st == (y,x-1) = Left
          | st == (y,x+1) = Right
          | st == (y-1,x) = Down
          | otherwise  = Up
        -- cond1 (X - cant go there)
        --          _ _ _ _ _
        --          _ _ W X W
        -- start -> W W W _ _
        --
        -- cond2 (X - cant go there)
        --          _ _ _ _ W W
        --          W W W W X _
        -- start -> W _ _ _ W W
        closerWall :: CellCoord -> Bool
        closerWall c@(y',x') = cond1 && cond2
          where
            cond1
              | dir c == Left = (tm !! y') !! (x'- 1) /= bwall
              | dir c == Right = (tm !! y') !! (x'+ 1) /= bwall
              | dir c == Up = (tm !! (y' + 1)) !! x' /= bwall
              | otherwise  = (tm !! (y' - 1)) !! x' /= bwall
            cond2
              | dir c == Left =
                  (tm !! (y' + 1)) !! x' /= bwall &&
                  (tm !! (y' - 1)) !! x' /= bwall &&
                  (tm !! (y' + 1)) !! (x' - 1) /= bwall &&
                  (tm !! (y' - 1)) !! (x' - 1) /= bwall
              | dir c == Right =
                  (tm !! (y' + 1)) !! x' /= bwall &&
                  (tm !! (y' - 1)) !! x' /= bwall &&
                  (tm !! (y' + 1)) !! (x' + 1) /= bwall &&
                  (tm !! (y' - 1)) !! (x' + 1) /= bwall
              | dir c == Up =
                  (tm !! y') !! (x' - 1) /= bwall &&
                  (tm !! y') !! (x' + 1) /= bwall &&
                  (tm !! (y' + 1)) !! (x' - 1) /= bwall &&
                  (tm !! (y' + 1)) !! (x' + 1) /= bwall
              | otherwise  =
                  (tm !! y') !! (x' - 1) /= bwall &&
                  (tm !! y') !! (x' + 1) /= bwall &&
                  (tm !! (y' - 1)) !! (x' - 1) /= bwall &&
                  (tm !! (y' - 1)) !! (x' + 1) /= bwall

-- | fill random count of random gaps between bwalls with dwalls
fillGapsBetweenWalls :: (RandomGen g) => g -> Tilemap -> Tilemap
fillGapsBetweenWalls g tm = placeOn (replicate count dwall) cells tm
  where
    gaps = gapBetweenWallsPlaces tm
    (count,g') = randomR (1,(length gaps - 1) `div` 3) g
    cells = takeRandom g' count gaps

----------------------------------------------------------------------------

-- | size of Tilemap == length of any contained list
gapPlaces :: Tilemap -> [CellCoord]
gapPlaces tm = filter (\c -> takeCellStr tm c == gap) allcoords
  where
    size = length tm
    allcoords = [(h,w) | h <- [0 .. size - 1], w <- [0 .. size - 1]]

-- | square = based on freeAround function (now 3x3)
gapSquarePlaces :: Tilemap -> [CellCoord]
gapSquarePlaces [] = []
gapSquarePlaces tm  = filter (freeAround tm) $ gapPlaces tm

-- | gaps like this:
-- _ _ _
-- W _ W
-- _ _ _
gapBetweenWallsPlaces :: Tilemap -> [CellCoord]
gapBetweenWallsPlaces tm =
  [cell | cell <- gaps, cond1 cell || cond2 cell]
  where
    gaps = gapPlaces tm
    cond1 (y,x) = takeCellStr tm (y-1,x) == bwall &&
                  takeCellStr tm (y+1,x) == bwall &&
                  takeCellStr tm (y,x-1) /= bwall &&
                  takeCellStr tm (y,x+1) /= bwall

    cond2 (y,x) = takeCellStr tm (y-1,x) /= bwall &&
                  takeCellStr tm (y+1,x) /= bwall &&
                  takeCellStr tm (y,x-1) == bwall &&
                  takeCellStr tm (y,x+1) == bwall

-- | it also returns chosen coord beside tilemap
placeSmthOnAnyFree :: (RandomGen g) => g -> TilemapCell ->
                      Tilemap -> (Tilemap, CellCoord)
placeSmthOnAnyFree gen obj tm = (newMap, gaps !! i)
  where
    gaps = gapPlaces tm
    i = fst $ randomR (0, length gaps - 1) gen
    newMap = writeCondition tm (gaps !! i) obj

-- | length [TilemapCell] == length [CellCoord]
placeOn :: [TilemapCell] -> [CellCoord] -> Tilemap -> Tilemap
placeOn [] _ tm = tm
placeOn _ [] tm = tm
placeOn (tc:tcs) (c:cs) tm = placeOn tcs cs $ writeCondition tm c tc

-- | place random count of buffs in different angles of the tilemap
placeSecretBuffs :: (RandomGen g) => g -> Tilemap -> Tilemap
placeSecretBuffs g tm = placeOn (takeRandom g'' count buffs) cells tm
  where
    gaps = [cell | cell <- gapPlaces tm,
                   length (elemIndices bwall $ candidates cell) >= 7]
    candidates cell = map (takeCellStr tm) (aroundPlace cell)
    (count,g') = randomR (1,4) g
    g'' = snd $ next g'
    cells = takeRandom g' count gaps

--------------------ENEMIES-------------------------------------------------

-- | Place enemies stacks
-- also place one buff near every stack
placeEnemies :: (RandomGen g) => g -> Int -> Tilemap -> Tilemap
placeEnemies g count tm =
  if mapWithNewStack == tm || count == 0
  then tm
  else placeEnemies g' (count - 1)  mapWithNewStack
  where
    mapWithNewStack = placeEnemyStackAndBuff g tm
    g' = snd $ next g

-- | it finds cell with freeAround == True and places stack of enemies here
placeEnemyStackAndBuff :: (RandomGen g) => g -> Tilemap -> Tilemap
placeEnemyStackAndBuff g tm =
  if variations == []
  then tm
  else writeCondition mapWithEnemies coord (buffs !! 0)
  where
    variations = gapSquarePlaces tm
    coord = variations !! i
    (i,g') = randomR (0, length variations - 1) g
    (c,g'') = randomR interv g' -- count of possible enemies
    interv = (1.0,3.0) :: (Float, Float)
    count = round c
    mapWithEnemies = placeEnemiesInArea g'' count coord tm

placeEnemiesInArea :: (RandomGen g) => g -> Int -> CellCoord ->
                      Tilemap -> Tilemap
placeEnemiesInArea g count c tm = placeOn enemies candidates tm
  where
    g' = snd $ next g
    candidates = takeRandom g' count $ aroundPlace c
    enemiesTypes = takeRandom g count list --digits: like 4 in e04
    list = ([1,2,3,4] :: [Int])
    enemies = map (\e -> typeEnemy e) enemiesTypes

--------------------PORTAL--------------------------------------------------
-- | place portal far enough from player
-- final position depends on 2-nd input parameter - length
placePortal :: (RandomGen g) => g -> CellCoord -> Int ->
               Tilemap -> Tilemap
placePortal g plCoord leng tm =
  if candidates /= []
  then writeCondition tm (candidates !! i) portal
  else placePortal g plCoord (leng - 1) tm
  where
    candidates = [c | c <- gapPlaces tm, far c plCoord]
    far :: CellCoord -> CellCoord -> Bool
    far (y1,x1) (y2,x2) =
      sqrt (fromIntegral $ (y1-y2)*(y1-y2) + (x1-x2)*(x1-x2))
        >= (fromIntegral leng)
    i = fst $ randomR (0, length candidates - 1) g
----------------------------------------------------------------------------
genTileMap :: Int -> IO (Tilemap)
genTileMap size = do
  -- build Walls
  g <- newStdGen
  let
    { sMap = startMap size
    ; bwallMap = iterBuildWalls g (size `div` 4) sMap
    ; g' = snd $ next g
    ; g'' = snd $ next g'
    ; g''' = snd $ next g''
    ; g'''' = snd $ next g'''
    }
   -- place enemy stacks and player
  let
    { eStacksCount = size `div` 4
    ; (plMap,plCoord) = placeSmthOnAnyFree g' player bwallMap
    ; eMap = placeEnemies g'' eStacksCount plMap
    ; portalMap = placePortal g' plCoord (size) eMap
    ; dMap = fillGapsBetweenWalls g''' portalMap
    ; finMap = placeSecretBuffs g'''' dMap
    }
  return (finMap)
