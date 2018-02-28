module Data where

data Player = Player
  { pPosX   :: Float
  , pPosY   :: Float
  , pRadian :: Float
  , pHp     :: Int
  , pSpeed  :: Float
  , pBuff   :: Int
  }

data Fireball = Fireball
  { fPosX   :: Float
  , fPosY   :: Float
  , fRadian :: Float
  , fDamage :: Int
  , fRadius :: Float
  , fSpeed  :: Float -- fSpeed < 2 fRadius
  , fModel  :: Int
  }

data Enemy = Enemy
  { ePosX   :: Float
  , ePosY   :: Float
  , eHp     :: Int
  , eDamage :: Int
  , eRange  :: Float
  , eSpeed  :: Float
  , eModel  :: Int
  , eTex    :: Int
  , eVision :: Float
  , eAgro   :: Bool
  }

data CellCond = Blocked | Free | Destructible

type CellCoord = (Int, Int)
type Tilemap = [[String]]

type Scene = (Player, [Fireball], [Enemy], Tilemap)
