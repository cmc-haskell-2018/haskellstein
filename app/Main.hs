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

--seems like it
gameLoop :: Scene -> String
gameLoop (_,_,[],_) = "VICTORY"
gameLoop (p,f,e,tmap)
    | isend         = "GAMEOVER"
    | otherwise     = ret
  where
    isend = (pHp p) < 0
    ret   = gameLoop . doEnemies . doFireballs . doPlayer $ (p, f, e, tmap)

--not used now
printEnemies :: [Enemy] -> IO()
printEnemies [] = putStrLn "end"
printEnemies (e:es) = do
    putStrLn $ "Enemy x=" ++ show (ePosX e) ++ " y=" ++ show (ePosY e)
    printEnemies es
