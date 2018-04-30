module Haskellstein where

import Haskellstein.Sftool
import Haskellstein.Raycaster
import Haskellstein.Initialize
import Haskellstein.Data
import Haskellstein.Picture
import Haskellstein.Interactions
import Control.Concurrent (threadDelay)

start :: [String] -> IO()
start args = do
    let argsLen = length args
    if argsLen == 0 then do
        putStrLn "Victory"
    else do
        tilemap      <- readFile $ head args
        let scene    = (createScene  (createTilemap tilemap)) {sArgs = args}
        let newScene = if (argsLen == 4) then (setElemsInMenu (scene {sState = Menu}) ["RESUME","RESTART", "SAVE", "START"] [False, False, False, True]) else scene
        res  <- greatCycle newScene getScene coolUpdateScene
        let levelEnd = fst res
        let newArgs  = snd res
        if (levelEnd == Victory) then start newArgs
        else putStrLn "Exit"



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
  -> (a -> Scene) --scene
  -> (a -> Control -> Float -> IO(a)) --updateScene
  -> IO((GameState, [String]))
greatCycle scene get update = do
  let scn = get scene
  if ((getState scn) == Victory) then
    return (Victory, (tail (sArgs scn)))
  else if ((sState scn) == Game) then do
          displayPicture (makePicture scn) True
          control      <- getControl
          delta        <- getDelta
          newScene     <- update scene control delta
          greatCycle newScene get update
  else if ((sState scn) == Menu) then do
          control      <- getControl
          delta        <- getDelta
          let
            menuState       = sMenuState scn
            menuTable       = sMenuTable scn in
            do
              displayMenu menuTable menuState
              newScene     <- update scene control delta
              greatCycle newScene get update
  else if ((sState scn) == Exit) then do
          return (Exit, [])
  else do
    return (Input, [])

menuTable2list :: MenuTable -> [Bool]
menuTable2list menuTable =
  [mStart menuTable, mResume menuTable, mRestart menuTable, mSave menuTable, mLoad menuTable, mExit menuTable]

updateMenuState :: MenuState -> Control -> Float -> MenuState
updateMenuState menuState control delta =
  let
    delay     = mDelay menuState
    timer     = fst delay
    timeBound = snd delay
    newTime   = getMaybeValue timer - delta
    enter     = cReturn control in
  if (timer == Nothing) then
    let
      bound = mIndexBound menuState
      currIdx = mIndex menuState
      newIdx  = updateIndex currIdx bound control
      newDelay = if (currIdx /= newIdx) then (Just timeBound, timeBound) else delay in
    menuState {mIndex = newIdx, mDelay = newDelay, mEnter = enter}
  else
    let newDelay = if (newTime <= 0.0) then (Nothing, timeBound) else (Just newTime, timeBound) in
  menuState {mDelay = newDelay, mEnter = enter}

updateIndex :: Int -> Int -> Control -> Int
updateIndex currIdx bound control =
  let
    upShift = if cForward control then (-1) else 0
    downShift = if cBack control then 1 else 0
    shift = upShift + downShift
    newIdx = currIdx + shift in
  if newIdx > bound then 0 else if newIdx < 0 then bound else newIdx

getMaybeValue :: (Fractional a) => Maybe a -> a
getMaybeValue Nothing = 0.0
getMaybeValue (Just x)  = x


--getScene
getScene :: Scene -> Scene
getScene scene = scene

--getSceneState
getState :: Scene -> GameState
getState scene =
  if ((pHp . sPlayer $ scene) <= 0) then Defeat
  else if (pExit . sPlayer $ scene) then Victory
  else sState scene

--visualizeScene
displayPicture :: Picture -> Bool -> IO()
displayPicture picture update = do
  setHealthBarSize (piHp picture)
  drawScene picture
  if update then updateWorkspace else return ()

--visualizeMenu
displayMenu :: MenuTable -> MenuState -> IO()
displayMenu menuTable menuState = do
  setHealthBarSize 0
  displayText menuContent (menuTable2list menuTable) (mIndex menuState)
  --drawText("Haskellstein v2.0", xOffset * 10, yOffset - 40, 35)
  updateWorkspace


xOffset :: Int
xOffset = 20

yOffset :: Int
yOffset = 45

lineHeight :: Int
lineHeight = 40

fontSize :: Int
fontSize = 40

columnSize :: Int
columnSize = 7
 
displayText :: [String] -> [Bool] -> Int -> IO()
displayText content isPrint index =
  let
    toPrint  = zip [0 .. ] $ map (\(_, str) -> str) $ filter (\(x, _) -> x) $ zip isPrint content
    function = \(idx, str) -> if (idx == index) then drawText ("->" ++ str ++ "<-", xOffset, yOffset + idx * lineHeight, fontSize) else drawText (str, xOffset, yOffset + idx * lineHeight, fontSize) in
    mapM_ function toPrint

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
  keymenuState      <- getKeyPressed keyM
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
                       (keymenuState /= 0)
                       (keyGState /= 0)
                       (keyReturnState /= 0)
  return newControl

--get delta time
getDelta :: IO(Float)
getDelta = do
  deltaTime <- getDeltaTime
  let delta = if (deltaTime > 0.1) then 0.1 else deltaTime
  return delta

coolUpdateScene :: Scene -> Control -> Float -> IO(Scene)
coolUpdateScene scene control delta =
  let
    sceneState = sState scene in
  if (getState scene) == Defeat then
    let
      player    = sPlayer scene
      newPlayer = player {pHp = 10}
      newScene  = (setElemsInMenu scene ["RESUME", "SAVE"] [False, False]) {sPlayer = newPlayer} in
    return (newScene {sState = Menu, sDelta = 0, sControl = control})
  else if and [sceneState == Game, not (cMenu control)] then
    return (doInteractions (scene {sControl = control, sDelta = delta}))
  else if cMenu control then
    return scene {sState = Menu}
  else
    let
      newMenuState   = updateMenuState (sMenuState scene) control delta
      menuTable      = menuTable2list $ sMenuTable scene 
      enter          = mEnter newMenuState
      index          = mIndex newMenuState
      realIdx        = getIndexOfContent menuTable index 0
      menuElem       = menuContent !! realIdx in
      if enter then
        changeScene scene menuElem
      else
        return scene {sMenuState = newMenuState}

getIndexOfContent :: [Bool] -> Int -> Int -> Int
getIndexOfContent menuTable idx realIdx =
  if not (head menuTable) then
    getIndexOfContent (tail menuTable) idx (realIdx + 1)
  else if idx == 0 then
    realIdx
  else
    getIndexOfContent (tail menuTable) (idx - 1) (realIdx + 1)

changeScene :: Scene -> String -> IO(Scene)
changeScene scene melem =
  if melem == "RESUME" then
    return scene {sState = Game}
  else if melem == "EXIT" then
    return scene {sState = Exit}
  else if melem == "RESTART" then do
    newScene <- loadScene ".toRestartScene.txt"
    return newScene
  else if melem == "SAVE" then do
    _ <- saveScene scene ".savedScene.txt"
    return (setElemInMenu scene "LOAD" True)
  else if melem == "START" then do
    let newScene = (setElemsInMenu scene ["RESUME","RESTART", "SAVE", "START"] [True, True, True, False]) {sState = Game}
    _ <- saveScene newScene ".toRestartScene.txt"
    _ <- saveScene newScene ".savedScene.txt"
    return newScene
  else do
    newScene <- loadScene ".savedScene.txt"
    return ((setElemsInMenu newScene ["RESUME","RESTART", "SAVE", "START"] [True, True, True, False]) {sState = Game})

setElemsInMenu :: Scene -> [String] -> [Bool] -> Scene
setElemsInMenu scene keys vals =
  if or [(length keys) /= (length vals), (length keys) == 0] then
    scene
  else
    setElemsInMenu (setElemInMenu scene (head keys) (head vals)) (tail keys) (tail vals)

setElemInMenu :: Scene -> String -> Bool -> Scene
setElemInMenu scene melem val =
  let
    menuTable              = sMenuTable scene
    (shift, newMenuTable)  = changeTable menuTable melem val
    menuState              = sMenuState scene
    newIndexBound          = mIndexBound menuState + shift
    newMenuState           = menuState {mIndexBound = newIndexBound, mIndex = 0} in
    scene {sMenuTable = newMenuTable, sMenuState = newMenuState}


changeTable :: MenuTable -> String -> Bool -> (Int, MenuTable)
changeTable menuTable telem val =
  let x = if val then 1 else (-1) in
  if telem == "START" then
    (if mStart menuTable == val then 0 else x, menuTable {mStart   = val})
  else if telem == "RESUME" then
    (if mResume menuTable == val then 0 else x, menuTable {mResume  = val})
  else if telem == "RESTART" then
    (if mRestart menuTable == val then 0 else x, menuTable {mRestart = val})
  else if telem == "SAVE" then
    (if mSave menuTable == val then 0 else x, menuTable {mSave    = val})
  else if telem == "LOAD" then
    (if mLoad menuTable == val then 0 else x, menuTable {mLoad    = val})
  else
    (if mExit menuTable == val then 0 else x, menuTable {mExit    = val})

saveScene :: Scene -> String -> IO()
saveScene scene file = do
  writeFile file (show scene)
  threadDelay 500000

loadScene :: String -> IO(Scene)
loadScene file = do
  scene <- readFile file  
  return (read scene :: Scene)

