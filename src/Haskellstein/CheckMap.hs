module Haskellstein.CheckMap where

import Prelude
import Haskellstein.Data

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