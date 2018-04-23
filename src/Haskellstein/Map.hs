module Haskellstein.Map where

import Prelude hiding (Right,Left)
import Haskellstein.Data
import Haskellstein.CheckMap
import System.Random

data Direction = Left | Right | Up | Down
  deriving (Eq,Show)

-- | dont understand why Maybe doesnt contain getter function
getMaybeVal :: Maybe a -> a
getMaybeVal (Just v) = v

checkCount :: Int
checkCount = 10

defMapSize :: Int
defMapSize = 16

wall :: String
wall = "W"

gap :: String
gap = "_"

printTileMap :: Tilemap -> IO ()
printTileMap tm = mapM_ (print) tm

-- | view: (example with size == 4)
-- w00 w00 w00 w00
-- w00 v00 v00 w00
-- w00 v00 v00 w00
-- w00 w00 w00 w00
startMap :: Int -> Tilemap
startMap size = modifyAtRectangle (\_ -> gap) (1,1) (size-2,size-2) s
  where
    s = replicate size $ replicate size wall

genStartCoords :: (RandomGen g) => g -> Int -> ([CellCoord], g)
genStartCoords gen size = (,) [coord1,coord2] g2 
  where
    (c1,g1) = randomR (2, size - 3) gen
    (c2,g2) = randomR (2, size - 3) g1
    coord1 = (c1, size `div` 3)
    coord2 = (c2, size - (size `div` 3))

freeAround :: Tilemap -> CellCoord -> Bool
freeAround tm (y,x) = not $ any (== False)
     [takeCellStr tm (h,w) /= wall | h <- [y-1 .. y+1], w <- [x-1 .. x+1]]

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
  then newMap
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

genTileMap :: Int -> IO ()
genTileMap size = do
  gen <- newStdGen
  let
    { sMap = startMap size
    ; (mFstCoord, g) = genStartCoord gen size checkCount sMap
    ; finMap = iterBuildWalls (getMaybeVal mFstCoord, g) 2 size sMap 
    }
  printTileMap finMap
