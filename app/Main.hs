module Main where

import Prelude
import System.Environment
import Data
import Initialize
import Interactions 

main :: IO ()
main = do
    args <- getArgs
    if null args then do
        putStrLn "No file path"
    else do
        src <- readFile . head $ args
        let scene  = createScene . createTilemap $ src
        putStrLn . gameLoop $ scene

gameLoop :: Scene -> String
gameLoop (p,f,e,tmap)
    | isend     = "GAMEOVER"
    | otherwise = ret
  where
    isend        = (pHp p) < 0
    (newp, newe) = stepEnemies p e tmap
    ret          = gameLoop (newp, f, newe, tmap)

printEnemies :: [Enemy] -> IO()
printEnemies [] = putStrLn "end"
printEnemies (e:es) = do
    putStrLn $ "Enemy x=" ++ show (ePosX e) ++ " y=" ++ show (ePosY e)
    printEnemies es
