module Main where

import Prelude
import System.Environment
import Data
import Initialize

main :: IO ()
main = do
    args <- getArgs
    if null args then do
        putStrLn "No file path"
    else do
        src <- readFile . head $ args
        let scene  = createScene . createTilemap $ src
        let (player, _, enemies, edit) = scene
        putStrLn $ "Player x=" ++ show (pPosX player) ++ " y=" ++ show (pPosY player)
        printEnemies enemies
        putStrLn $ "edited map " ++ show edit

printEnemies :: [Enemy] -> IO()
printEnemies [] = putStrLn "end"
printEnemies (e:es) = do
    putStrLn $ "Enemy x=" ++ show (ePosX e) ++ " y=" ++ show (ePosY e)
    printEnemies es
