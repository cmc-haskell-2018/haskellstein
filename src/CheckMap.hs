module CheckMap where

import Data
import Data.List.Index
import Prelude

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
