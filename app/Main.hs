module Main where

import Prelude
import System.Environment
import System.IO.Unsafe
import Data
import Initialize
import Interactions 

main :: IO ()
main = do
    args <- getArgs
    if null args then do
        putStrLn "No file path"
    else do
        let src = argsToMaps args
        let scene  = createScene . createTilemap . head $ src
        putStrLn . gameLoop $ scene

--seems like it
gameLoop :: Scene -> String
gameLoop (_,_,[],_) = "VICTORY"
gameLoop (p,f,e,tmap)
    | isend         = "GAMEOVER"
    | otherwise     = gameLoop ret
  where
    isend = (pHp p) < 0
    ret   = doInteractions (p, f, e, tmap)

--create map list
argsToMaps :: [String] -> [String]
argsToMaps []     = []
argsToMaps (x:xs) = tmap : argsToMaps xs
  where
    tmap = unsafePerformIO . readFile $ x

--not used now
printEnemies :: [Enemy] -> IO()
printEnemies [] = putStrLn "end"
printEnemies (e:es) = do
    putStrLn $ "Enemy x=" ++ show (ePosX e) ++ " y=" ++ show (ePosY e)
    printEnemies es
