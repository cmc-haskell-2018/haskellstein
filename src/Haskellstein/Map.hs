module Haskellstein.Map where

import Prelude hiding (Right,Left)
import Data.List (delete)
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

wall :: String
wall = "w01"

gap :: String
gap = "v00"

buff :: String
buff = "i04"

portal :: String
portal = "b00"

typeEnemy :: Int -> String
typeEnemy e = "e0" ++ show e

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
    s = replicate size $ replicate size wall

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

-- | square around given cell
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


-- | algorithm of building chains of walls
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
    startStepWalls = writeCondition tm coord wall
    newMap = buildWalls g coord startStepWalls

buildWalls :: (RandomGen g) => g -> CellCoord ->
              Tilemap -> Tilemap
buildWalls gen fstCoord tmap = stepWalls gen fstCoord tmap
  where
    stepWalls :: (RandomGen g) => g -> CellCoord ->
                Tilemap -> Tilemap
    stepWalls g (y,x) tm =
      if way /= []
      then stepWalls g'' step $ writeCondition tm step wall
      else tm
      where
        variations = [(y,x-1),(y,x+1),(y-1,x),(y+1,x)]
        way = [(n,m) | (n,m) <- variations,
                (tm !! n) !! m /= wall && closerWall (n,m)]
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
              | dir c == Left = (tm !! y') !! (x'- 1) /= wall
              | dir c == Right = (tm !! y') !! (x'+ 1) /= wall
              | dir c == Up = (tm !! (y' + 1)) !! x' /= wall
              | otherwise  = (tm !! (y' - 1)) !! x' /= wall
            cond2
              | dir c == Left =
                  (tm !! (y' + 1)) !! x' /= wall &&
                  (tm !! (y' - 1)) !! x' /= wall &&
                  (tm !! (y' + 1)) !! (x' - 1) /= wall &&
                  (tm !! (y' - 1)) !! (x' - 1) /= wall
              | dir c == Right =
                  (tm !! (y' + 1)) !! x' /= wall &&
                  (tm !! (y' - 1)) !! x' /= wall &&
                  (tm !! (y' + 1)) !! (x' + 1) /= wall &&
                  (tm !! (y' - 1)) !! (x' + 1) /= wall
              | dir c == Up =
                  (tm !! y') !! (x' - 1) /= wall &&
                  (tm !! y') !! (x' + 1) /= wall &&
                  (tm !! (y' + 1)) !! (x' - 1) /= wall &&
                  (tm !! (y' + 1)) !! (x' + 1) /= wall
              | otherwise  =
                  (tm !! y') !! (x' - 1) /= wall &&
                  (tm !! y') !! (x' + 1) /= wall &&
                  (tm !! (y' - 1)) !! (x' - 1) /= wall &&
                  (tm !! (y' - 1)) !! (x' + 1) /= wall

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

placeSmthOnAnyFree :: (RandomGen g) => g -> TilemapCell ->
                      Tilemap -> Tilemap
placeSmthOnAnyFree gen obj tm = newMap 
  where
    gaps = gapPlaces tm 
    i = fst $ randomR (0, length gaps - 1) gen
    newMap = writeCondition tm (gaps !! i) obj
    
-- | length [TilemapCell] == length [CellCoord]
placeOn :: [TilemapCell] -> [CellCoord] -> Tilemap -> Tilemap
placeOn [] _ tm = tm
placeOn _ [] tm = tm
placeOn (tc:tcs) (c:cs) tm = placeOn tcs cs $ writeCondition tm c tc

--------------------ENEMIES----------------------------------------------- 

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
  else writeCondition mapWithEnemies coord buff
  where
    variations = gapSquarePlaces tm 
    coord = variations !! i
    (i,g') = randomR (0, length variations - 1) g
    (c,g'') = randomR interv g' -- count of possible enemies    
    interv = (2.0,4.0) :: (Float, Float)
    count = floor c
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

-------------------------------------------------------------------------- 
genTileMap :: Int -> IO (Tilemap)
genTileMap size = do
  -- build Walls
  g <- newStdGen
  let
    { sMap = startMap size
    ; wallMap = iterBuildWalls g (size `div` 5) sMap 
    ; g' = snd $ next g
    ; g'' = snd $ next g'
    ; g''' = snd $ next g''
    }
   -- place enemy stacks and player
  let
    { eCount = size `div` 5
    ; finMap =  placeSmthOnAnyFree g''' portal $
                placeEnemies g'' eCount $
                placeSmthOnAnyFree g' player wallMap
    }
  return (finMap)
