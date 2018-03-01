module CheckMap where

import Data
import Prelude

--specify step-on-cell perfomance
specCellCond :: Tilemap -> CellCoord -> CellCond
specCellCond tm (n,m) = spec (head (dropElements m
                                    (head (dropElements n tm))))
  where
    dropElements :: Int -> [a] -> [a]
    dropElements _ [] = []
    dropElements count (x:xs)
        | count == 0 = (x:xs)
        | otherwise = dropElements (count - 1) xs
    spec :: String -> CellCond
    spec c
        | head c == 'w' || head c == 'a' || head c == 'o' = Blocked
        | head c == 'v' || head c == 'i' || head c == 'b' = Free
        | otherwise = Destructible
