module Haskellstein.Data where

type Position = (Float, Float) -- (x,y)

type Cooldown = (Maybe Float, Float) --(current time, cooldown)

data EnemyType = Melee | Range | Mage | Demon
    deriving (Eq, Show, Read)

data FireballType = Small | Elec
    deriving (Eq, Show, Read)

data ColorTex = Red | Green | Blue | Normal
    deriving (Eq, Show, Read)

type ObjectTexture = Int

data Player = Player
  { pPos        :: Position
  , pRadian     :: Float
  , pHp         :: Int
  , pSpeed      :: Float
  , pASpeed     :: Cooldown
  , pDamage     :: Int
  , pTurnAround :: Maybe Float
  , pExit       :: Bool
  , pElec       :: Bool
  , pFType      :: FireballType
  } deriving (Read, Show)

data Fireball = Fireball
  { fPos    :: Position
  , fRadian :: Float
  , fDamage :: Int
  , fRadius :: Float
  , fSpeed  :: Float
  , fModel  :: FireballType
  , fTex    :: ObjectTexture
  , fAnim   :: Cooldown
  , fColor  :: ColorTex
  } deriving (Read, Show)

data Enemy = Enemy
  { ePos    :: Position
  , eHp     :: Int
  , eDamage :: Int
  , eRange  :: Float
  , eSpeed  :: Float
  , eASpeed :: Cooldown
  , eModel  :: EnemyType
  , eTex    :: ObjectTexture
  , eVision :: Float
  , eAgro   :: Bool
  , eAnim   :: Cooldown
  , eMoved  :: Bool
  , eColor  :: ColorTex
  , eBlood  :: Cooldown
  } deriving (Read, Show)

data CellCond = Blocked | Free | Destructible
    deriving (Eq)

type CellCoord = (Int, Int) -- (y,x)

type TilemapCell = String

type Tilemap = [[TilemapCell]]

data Control = Control
    { cForward    :: Bool
    , cBack       :: Bool
    , cLeftM      :: Bool
    , cRightM     :: Bool
    , cLeftT      :: Bool
    , cRightT     :: Bool
    , cSpace      :: Bool
    , cTurnAround :: Bool
    , c1          :: Bool
    , c2          :: Bool
    , cMenu       :: Bool
    , cGame       :: Bool
    , cReturn     :: Bool
    } deriving (Read, Show)

data Scene = Scene
    { sPlayer        :: Player
    , sFireball      :: [Fireball]
    , sEnemy         :: [Enemy]
    , sTilemap       :: Tilemap
    , sControl       :: Control
    , sDelta         :: Float
    , sTextureCond   :: TexTimer
    , sDeadEnemy     :: [Enemy]
    , sEnemyFireball :: [Fireball]
    , sState         :: GameState
    , sArgs          :: [String]
    , sMenuState     :: MenuState
    } deriving (Show, Read)

data MenuState = MenuState
    { mIndex           :: Int
    , mIndexBound      :: Int
    , mEnter           :: Bool
    , mTable           :: MenuTable
    , mDelay           :: Cooldown
    } deriving (Show, Read)

data MenuTable = MenuTable
    { mStart          :: Bool
    , mResume         :: Bool
    , mRestart        :: Bool
    , mSave           :: Bool    
    , mLoad           :: Bool
    , mExit           :: Bool
    } deriving (Show, Read)

menuContent :: [String]
menuContent =  ["START", "RESUME", "RESTART", "SAVE", "LOAD", "EXIT"]

type TexTimer = Maybe Float

data GameState = Menu | Game | Victory | Defeat | Exit
    deriving (Eq, Show, Read)