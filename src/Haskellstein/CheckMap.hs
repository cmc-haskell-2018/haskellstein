module Haskellstein.CheckMap where

import Prelude
import Haskellstein.Data
import Haskellstein.Texconsts

--specify step-on-cell perfomance
specCellCond :: Tilemap -> CellCoord -> CellCond
specCellCond tm (n,m) = spec (head (dropElements m
                                    (head (dropElements n tm))))
  where
    dropElements :: Int -> [a] -> [a]
    dropElements _ [] = []
    dropElements count (x:xs)
        | count == 0  = (x:xs)
        | otherwise   = dropElements (count - 1) xs
    spec :: String -> CellCond
    spec c
        | head c == 'w' || head c == 'a' || head c == 'o' = Blocked
        | head c == 'v' || head c == 'i' || head c == 'b' = Free
        | otherwise                                       = Destructible

modifyAt :: Int -> (a -> a) -> [a] -> [a]
modifyAt i f ls
  | i < 0     = ls
  | otherwise = go i ls
  where
    go 0 (x:xs) = f x : xs
    go n (x:xs) = x : go (n-1) xs
    go _ []     = []
{-# INLINE modifyAt #-}

--change cell followed coordinates
writeCondition :: Tilemap -> CellCoord -> String -> Tilemap
writeCondition tm (n,m) str = modifyAt n (changeCell) tm
    where
      changeCell :: [String] -> [String]
      changeCell = modifyAt m (\_ -> str)

--remove if destructible
writeEmpty :: Tilemap -> CellCoord -> Tilemap
writeEmpty tm (n,m) = if head ((tm !! n) !! m) == 'd'
                          then writeCondition tm (n,m) "v00"
                          else tm

--take element
takeCellStr :: Tilemap -> CellCoord -> String
takeCellStr tmap (y,x) = (tmap !! y) !! x

--exit pisition
exitCell :: Tilemap -> CellCoord -> Bool
exitCell tmap coord
    | str == "b00" = True
    | str == "b01" = True
    | str == "b02" = True
    | str == "b03" = True
    | otherwise    = False
  where
    str = takeCellStr tmap coord

--potion pisition
potionCell :: Tilemap -> CellCoord -> (Tilemap, Bool)
potionCell tmap coord
    | str == "i04" = ((writeCondition tmap coord "v00"), True)
    | otherwise    = (tmap, False)
  where
    str = takeCellStr tmap coord

--elec pisition
elecCell :: Tilemap -> CellCoord -> (Tilemap, Bool)
elecCell tmap coord
    | str == "i08" = ((writeCondition tmap coord "v00"), True)
    | str == "i09" = ((writeCondition tmap coord "v00"), True)
    | otherwise    = (tmap, False)
  where
    str = takeCellStr tmap coord

--return move coords
getNewCoord :: Position -> Position -> Tilemap -> Position
getNewCoord (px,py) (newX,newY) tmap =
    if (ifXY == Free) then (newX, newY)
    else if (ifX == Free) then (newX, py)
         else if (ifY == Free) then (px, newY)
              else (px, py)
  where
    ifX  = specCellCond tmap (floor py, floor newX)
    ifY  = specCellCond tmap (floor newY, floor px)
    ifXY = specCellCond tmap (floor newY, floor newX)

--animation of tilemap cells
changeTextures :: Tilemap -> TexTimer -> Float -> (Tilemap, TexTimer)
changeTextures tm t delta =
  case t of
    Nothing  -> (map (map (chT)) tm, Just texCooldown)
    Just val -> (tm, newt val)
  where
    newt val = case (val - delta) < 0 of
      True  -> Nothing
      False -> Just (val - delta)
    chT :: TilemapCell -> TilemapCell
    chT c = case c of
      "b00" -> "b01"
      "b01" -> "b02"
      "b02" -> "b03"
      "b03" -> "b00"
      _     -> c
