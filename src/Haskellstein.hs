module Haskellstein where

import Haskellstein.Sftool
import Haskellstein.Raycaster
import Haskellstein.Initialize
import Haskellstein.Data
import Haskellstein.Picture
import Haskellstein.Interactions
import Control.Concurrent (threadDelay)
import System.IO

start :: [String] -> IO()
start args = do
    if length args == 0 then do
        putStrLn "Victory"
    else do
        let newArgs = tail args
        tilemap   <- readFile $ head args
        levelEnd  <- greatCycle (createScene  (createTilemap tilemap)) {sArgs = args}
                                stepScene
                                updateScene
                                endCheck
                                makePicture
                                getState
                                args
                                getScene
        if levelEnd then start newArgs
        else putStrLn "Defeat"

windowInit :: IO()
windowInit = do
               let
                 windowWidth       = 800
                 windowHeight      = 600
                 windowTitle       = "Haskellstein"
                 windowFrameLimit  = 100
                 scaleFactor       = 1
                 wallTexturePath   = "data/textures/wall.png"
                 enemyTexturePath  = "data/textures/enemy1.png"
                 spriteTexturePath = "data/textures/sprite1.png"
                 shaderPath        = "data/shader.frag"
                 fontPath          = "data/font.ttf"
                 musicPath         = "data/music.ogg"
               initWorkspace
                 (windowWidth, windowHeight, windowTitle,
                   windowFrameLimit, scaleFactor)
                 (wallTexturePath, enemyTexturePath, spriteTexturePath)
                 (shaderPath, fontPath, musicPath)

--GameLoop
greatCycle
  :: a --object
  -> (MenuState -> a -> IO(a)) --stepObject
  -> (a -> Control -> Float -> a) --getControlAndDelta
  -> (a -> GameState) --checkEndCondition
  -> (a -> Picture) --drawObject
  -> (a -> GameState) --sceneState
  -> [String] --args
  -> (a -> Scene) --scene
  -> IO(Bool)
greatCycle scene step update end picture state args get =
  if ((end scene) == Victory) then return True
  else if ((end scene) == Defeat) then return False
  else if ((state scene) == Game) then do
          displayPicture $ picture scene
          control      <- getControl
          delta        <- getDelta
          let
            scn      = get scene in
            do
              newScene <- step (sMenuState scn) $ update scene control delta
              greatCycle newScene step update end picture state args get
  else do
          control      <- getControl
          delta        <- getDelta
          let
            scn          = (get scene)
            mstate       = sMenuState scn
            mtable       = mTable mstate
            newMState    = getMenuState (menuTable2list mtable) mstate control delta in
            do
              displayMenu mtable newMState
              newScene <- step newMState $ update scene control delta
              greatCycle newScene step update end picture state args get

--doMenuActions :: Control -> Scene
--doMenuActions control =

menuTable2list :: MenuTable -> [Bool]
menuTable2list mtable =
  [mStart mtable, mResume mtable, mRestart mtable, mSave mtable, mLoad mtable, mExit mtable]

getMenuState :: [Bool] -> MenuState -> Control -> Float -> MenuState
getMenuState mtable mstate control delta =
  let
    delay = mDelay mstate
    timer = fst delay
    enter = cReturn control
  in if (timer == Nothing) then let
    bound = length [ x| x <- mtable, x == True ] - 1
    currIdx = mIndex mstate
    newIdx  = updateIndex currIdx bound control
    newDelay = ((Just (snd delay)), snd delay) in
    mstate {mIndex = newIdx, mDelay = newDelay, mEnter = enter}
  else let newDelay = if (((getMaybeValue timer) - delta) <= 0.0) then (Nothing, snd delay) else (Just ((getMaybeValue timer) - delta), snd delay) in
    mstate {mDelay = newDelay, mEnter = enter}

updateIndex :: Int -> Int -> Control -> Int
updateIndex currIdx bound control =
  let
    upShift = if cForward control then (-1) else 0
    downShift = if cBack control then 1 else 0
    shift = upShift + downShift
    newIdx = currIdx + shift
    agree = if and [(newIdx <= bound), (newIdx > -1)] then True else False in
  if agree then newIdx else currIdx

getMaybeValue :: (Fractional a) => Maybe a -> a
getMaybeValue Nothing = 0.0
getMaybeValue (Just x)  = x


--getScene
getScene :: Scene -> Scene
getScene scene = scene

--getSceneState
getState :: Scene -> GameState
getState scene = sState scene

--visualizeScene
displayPicture :: Picture -> IO()
displayPicture picture = do
  setHealthBarSize (piHp picture)
  drawScene picture
  updateWorkspace

--visualizeMenu
displayMenu :: MenuTable -> MenuState -> IO()
displayMenu mtable mstate = do
  setHealthBarSize 0
  displayText menuContent (menuTable2list mtable) (mIndex mstate) 0
  drawText("Haskellstein v2.0", xOffset * 10, yOffset - 40, 35)
  updateWorkspace


xOffset :: Int
xOffset = 20

yOffset :: Int
yOffset = 45

lineHeight :: Int
lineHeight = 25

fontSize :: Int
fontSize = 20

columnSize :: Int
columnSize = 7

displayText :: [String] -> [Bool] -> Int -> Int -> IO()
displayText str agree idx line =
  if (and [((length agree) /= 0), (head agree)])  then do
      if idx == line then
        drawText("*" ++ (head str), xOffset, yOffset + line * lineHeight, fontSize)
      else
        drawText("-" ++ (head str), xOffset, yOffset + line * lineHeight, fontSize)
      displayText (tail str) (tail agree) idx (line + 1)
  else if ((length agree) /= 0) then do
      displayText (tail str) (tail agree) idx line
  else do
      return ()


xor :: Bool -> Bool -> Bool
xor True a    = not a
xor False a   = a

--read pushed keys
getControl :: IO(Control)
getControl = do
  keyWState      <- getKeyPressed keyW
  keySState      <- getKeyPressed keyS
  keyAState      <- getKeyPressed keyA
  keyDState      <- getKeyPressed keyD
  keyQState      <- getKeyPressed keyLeftArrow
  keyEState      <- getKeyPressed keyRightArrow
  keySpaceState  <- getKeyPressed keySpace
  keyTurn        <- getKeyPressed keyI
  key1State      <- getKeyPressed key1
  key2State      <- getKeyPressed key2
  keyMState      <- getKeyPressed keyM
  keyGState      <- getKeyPressed keyG
  keyReturnState <- getKeyPressed keyReturn
  let newControl = Control
                       (keyWState /= 0)
                       (keySState /= 0)
                       (keyAState /= 0)
                       (keyDState /= 0)
                       (keyQState /= 0)
                       (keyEState /= 0)
                       (keySpaceState /= 0)
                       (keyTurn /= 0)
                       (key1State /= 0)
                       (key2State /= 0)
                       (keyMState /= 0)
                       (keyGState /= 0)
                       (keyReturnState /= 0)
  return newControl

--get delta time
getDelta :: IO(Float)
getDelta = do
  deltaTime <- getDeltaTime
  let delta = if (deltaTime > 0.1) then 0.1 else deltaTime
  return delta

switchScene :: Scene -> Control -> Scene
switchScene scene control =
  if cMenu control then scene {sState = Menu} else if cGame control then scene {sState = Game} else scene

--addExternalData
updateScene :: Scene -> Control -> Float -> Scene
updateScene scene control delta =
  let newScene = switchScene scene control in
  if sState newScene == Menu then newScene {sControl = control, sDelta = 0} else newScene {sControl = control, sDelta = delta}

--makeInteractions
endCheck :: Scene -> GameState
endCheck scene =
  if ((pHp . sPlayer $ scene) <= 0) then Defeat
  else if (pExit . sPlayer $ scene) then Victory
  else if ((sState scene) == Exit) then Defeat
  else Game

--makeInteractions
stepScene :: MenuState -> Scene -> IO(Scene)
stepScene mstate scene =
  let
    enter = mEnter mstate in
  if enter then
    updateSceneFromMenu scene mstate
  else if sState scene == Game then return (doInteractions scene) else return (scene {sMenuState = mstate})


updateSceneFromMenu :: Scene -> MenuState -> IO(Scene)
updateSceneFromMenu scene mstate =
  let
    index    = mIndex mstate
    mtable   = menuTable2list (mTable mstate)
    realIdx  = getIndexOfContent index mtable 0
    menuElem = menuContent !! realIdx in
    changeScene scene menuElem

changeScene :: Scene -> String -> IO(Scene)
changeScene scene melem =
  if melem == "RESUME" then
    return scene {sState = Game}
  else if melem == "EXIT" then
    return scene {sState = Exit}
  else if melem == "RESTART" then do
    newScene <- loadScene "toRestartScene.txt"
    return ((setElemsInMenu newScene ["RESUME","RESTART", "SAVE", "START"] [True, True, True, False]) {sState = Game})
  else if melem == "SAVE" then do
    _ <- saveScene scene "savedScene.txt"
    return scene
  else if melem == "START" then do
    _ <- saveScene scene "toRestartScene.txt"
    return ((setElemsInMenu scene ["RESUME","RESTART", "SAVE", "START"] [True, True, True, False]) {sState = Game})
  else do
    newScene <- loadScene "savedScene.txt"
    return ((setElemsInMenu newScene ["RESUME","RESTART", "SAVE", "START"] [True, True, True, False]) {sState = Game})


setElemsInMenu :: Scene -> [String] -> [Bool] -> Scene
setElemsInMenu scene keys vals =
  if or [(length keys) /= (length vals), (length keys) == 0] then
    scene
  else
    setElemsInMenu (setElemInMenu scene (head keys) (head vals)) (tail keys) (tail vals)

loadScene :: String -> IO(Scene)
loadScene file = do
  scene <- readFile file  
  return (read (preprocessing scene) :: Scene)

preprocessing :: String -> String
preprocessing str = str


setElemInMenu :: Scene -> String -> Bool -> Scene
setElemInMenu scene melem val =
  let
    mstate = sMenuState scene
    mtable = mTable mstate
    newMtable = changeTable mtable melem val
    newMstate = mstate {mTable = newMtable} in
    scene {sMenuState = newMstate}

changeTable :: MenuTable -> String -> Bool -> MenuTable
changeTable mtable telem val =
  if telem == "START" then
    mtable {mStart   = val}
  else if telem == "RESUME" then
    mtable {mResume  = val}
  else if telem == "RESTART" then
    mtable {mRestart = val}
  else if telem == "SAVE" then
    mtable {mSave    = val}
  else if telem == "LOAD" then
    mtable {mLoad    = val}
  else
    mtable {mExit    = val}


saveScene :: Scene -> String -> IO()
saveScene scene file = do
  writeFile file (show scene)
  threadDelay 500000

getIndexOfContent :: Int -> [Bool] -> Int -> Int
getIndexOfContent indx mtable realIdx =
  if and [(head mtable), indx > 0] then
    getIndexOfContent (indx - 1) (tail mtable) (realIdx + 1)
  else if (head mtable) then
    realIdx
  else
    getIndexOfContent indx (tail mtable) (realIdx + 1)
