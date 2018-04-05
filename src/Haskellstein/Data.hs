module Haskellstein.Data where

type Position = (Float, Float) -- (x,y)

type Cooldown = (Maybe Float, Float) --(current time, cooldown)

data EnemyType = Melee | Range | Mage | Demon
    deriving (Eq)

data FireballType = Small | Frost
    deriving (Eq)

data ColorTex = Red | Blue | Normal
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

