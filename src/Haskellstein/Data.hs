module Haskellstein.Data where

type Position = (Float, Float) -- (x,y)

type Cooldown = (Maybe Float, Float) --(current time, cooldown)

data EnemyType = Melee | Range | Mage | Demon
    deriving (Eq)

data FireballType = Small | Elec
    deriving (Eq)

data ColorTex = Red | Green | Blue | Normal
    deriving (Eq)

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
  }

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
  }

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
  }

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
    }

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
    }

type TexTimer = Maybe Float

data GameEnd = Victory | Defeat | Continue
    deriving (Eq)

