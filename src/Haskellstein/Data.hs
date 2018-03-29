module Haskellstein.Data where

type Position = (Float, Float) -- (x,y)

type Cooldown = (Maybe Float, Float) --(current time, cooldown)

data EnemyType = Melee | Range | Mage
    deriving (Eq)

data FireballType = Small | Huge
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
  }

data CellCond = Blocked | Free | Destructible
    deriving (Eq)

type CellCoord = (Int, Int) -- (y,x)

type TilemapCell = String

type Tilemap = [[TilemapCell]]

data Control = Control
    { cForward    :: Bool
    , cLeftT      :: Bool
    , cBack       :: Bool
    , cRightT     :: Bool
    , cLeftM      :: Bool
    , cRightM     :: Bool
    , cSpace      :: Bool
    , cTurnAround :: Bool
    }

data Scene = Scene
    { sPlayer   :: Player
    , sFireball :: [Fireball]
    , sEnemy    :: [Enemy]
    , sTilemap  :: Tilemap
    , sControl  :: Control
    , sDelta    :: Float
    }

data GameEnd = Victory | Defeat | Continue
    deriving (Eq)
