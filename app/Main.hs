module Main where

import Prelude
import System.Environment
import Data
import Initialize
import CheckMap

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
        let {xcoord = 3; ycoord = 5}
        putStrLn $ "edited map " ++ show edit
        putStr $ "Cell [" ++ show xcoord ++
                  ", " ++ show ycoord ++ "] is "
        case (specCellCond edit (xcoord,ycoord)) of 
          Blocked -> putStrLn "Blocked cell"
          Free -> putStrLn "Free cell"
          Destructible -> putStrLn "Destructible cell"

printEnemies :: [Enemy] -> IO()
printEnemies [] = putStrLn "end"
printEnemies (e:es) = do
    putStrLn $ "Enemy x=" ++ show (ePosX e) ++ " y=" ++ show (ePosY e)
    printEnemies es
