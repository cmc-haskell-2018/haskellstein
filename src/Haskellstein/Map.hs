module Haskellstein.Map where

import Prelude hiding (Right,Left)
import Data.List (delete)
import Haskellstein.Data
import Haskellstein.CheckMap
import System.Random

-- ! Approach !: Tilemap can be only square

data Direction = Left | Right | Up | Down
  deriving (Eq,Show)

-- | dont understand why Maybe doesnt contain getter function
-- obviously you should avoid Nothing input
getMaybeVal :: Maybe a -> a
getMaybeVal (Just v) = v
getMaybeVal Nothing  = undefined

checkCount :: Int
checkCount = 10

defMapSize :: Int
defMapSize = 16

wall :: String
wall = "w00"

gap :: String
gap = "v00"

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
  else val : takeRandom g' (count-1) (delete val l)
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
genStartCoord :: (RandomGen g) => g -> Int -> Int -> Tilemap ->
                  (Maybe CellCoord, g)
genStartCoord gen size i tm =
  if freeAround tm coord
  then (Just coord, g2)
  else if i == 0
       then (Nothing,g2)
       else genStartCoord g2 size (i-1) tm
  where
    (y1,g1) = randomR (2, size - 3) gen
    (x1,g2) = randomR (2, size - 3) g1
    coord = (y1,x1)


-- | algorithm of building chains of walls
iterBuildWalls :: (RandomGen g) => (CellCoord, g) -> Int -> Int ->
                  Tilemap -> Tilemap
iterBuildWalls (coord,gen) chainCount size tm =
  if chainCount == 0 || mNewCoord == Nothing
  then tm
  else iterBuildWalls (getMaybeVal mNewCoord, g)
                        (chainCount - 1) size newMap
  where
    (mNewCoord,g) = genStartCoord gen size checkCount newMap
    startStepWalls = writeCondition tm coord wall
    newMap = buildWalls gen coord startStepWalls

-- | minimum tilemap's size == 5
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
        -- _ _ _ _
        -- _ W X W
        -- _ W _ _
        -- cond2 (X - cant go there)
        -- _ _ _ _ W W
        -- W W W W X _
        -- W _ _ _ W W
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

-- | it finds cell with freeAround == True and places stack of enemies here
placeEnemyStack :: (RandomGen g) => g -> Tilemap -> Tilemap
placeEnemyStack g tm =
  if variations == []
  then tm
  else placeEnemiesInArea g'' count (variations !! i) tm
  where
    variations = filter (freeAround tm) (gapPlaces tm)
    (i,g') = randomR (0, length variations - 1) g
    (count,g'') = randomR (2, 4) g' -- count of possible enemies

placeEnemiesInArea :: (RandomGen g) => g -> Int -> CellCoord ->
                      Tilemap -> Tilemap
placeEnemiesInArea g count c tm = placeOn enemies candidates tm 
  where
    g' = snd $ next g
    candidates = takeRandom g' count $ aroundPlace c 
    enemiesTypes = takeRandom g count list -- digits: like 4 in e04
    enemies = map (\e -> "e0" ++ show e) enemiesTypes
    list = foldr1 (++) (replicate count [1..4])

-------------------------------------------------------------------------- 
genTileMap :: Int -> IO (Tilemap)
genTileMap size = do
  -- build Walls
  gen <- newStdGen
  let
    { sMap = startMap size
    ; (mFstCoord, g) = genStartCoord gen size checkCount sMap
    ; wallMap = iterBuildWalls (getMaybeVal mFstCoord, g) (size `div` 4)
                                    size sMap 
    }
   -- place one enemy and player
  gen2 <- newStdGen
  gen3 <- newStdGen
  let
    { finMap = placeEnemyStack gen3 $ placeSmthOnAnyFree gen2 "p00" wallMap
    }
  return (finMap)
