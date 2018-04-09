module Haskellstein.Texwork where

import Haskellstein.Data
import Haskellstein.Texconsts

--set attack texture
setAT :: ObjectTexture -> ObjectTexture
setAT tex
  | tex == meleeTex1 = meleeTexAttack1
  | tex == meleeTex2 = meleeTexAttack1
  | tex == meleeTex3 = meleeTexAttack1
  | tex == meleeTex4 = meleeTexAttack1
  | tex == rangeTex1 = rangeTexAttack
  | tex == rangeTex2 = rangeTexAttack
  | tex == mageTex1  = mageTexAttack
  | tex == mageTex2  = mageTexAttack
  | tex == demonTex1 = demonTexAttack1
  | tex == demonTex2 = demonTexAttack1
  | tex == demonTex3 = demonTexAttack1
  | tex == demonTex4 = demonTexAttack1
  | otherwise        = meleeTexAttack1

--set death texture
setDT :: ObjectTexture -> ObjectTexture
setDT tex
  | tex == meleeTex1       = meleeTexDeath1
  | tex == meleeTex2       = meleeTexDeath1
  | tex == meleeTex3       = meleeTexDeath1
  | tex == meleeTex4       = meleeTexDeath1
  | tex == mageTex1        = mageTexDeath1
  | tex == mageTex2        = mageTexDeath1
  | tex == rangeTex1       = rangeTexDeath1
  | tex == rangeTex2       = rangeTexDeath1
  | tex == demonTex1       = demonTexDeath1
  | tex == demonTex2       = demonTexDeath1
  | tex == demonTex3       = demonTexDeath1
  | tex == demonTex4       = demonTexDeath1
  | tex == meleeTexAttack1 = meleeTexDeath1
  | tex == meleeTexAttack2 = meleeTexDeath1
  | tex == mageTexAttack   = mageTexDeath1
  | tex == rangeTexAttack  = rangeTexDeath1
  | tex == demonTexAttack1 = demonTexDeath1
  | tex == demonTexAttack2 = demonTexDeath1
  | tex == demonTexAttack3 = demonTexDeath1
  | otherwise              = meleeTexDeath1

--swap enemy texture
swapET :: ObjectTexture -> ObjectTexture
swapET tex
  | tex == meleeTexAttack1 = meleeTexAttack2
  | tex == meleeTexAttack2 = meleeTex1
  | tex == rangeTexAttack  = rangeTex1
  | tex == mageTexAttack   = mageTex1
  | tex == demonTexAttack1 = demonTexAttack2
  | tex == demonTexAttack2 = demonTexAttack3
  | tex == demonTexAttack3 = demonTex3
  | tex == meleeTex1       = meleeTex2
  | tex == meleeTex2       = meleeTex3
  | tex == meleeTex3       = meleeTex4
  | tex == meleeTex4       = meleeTex1
  | tex == rangeTex1       = rangeTex2
  | tex == rangeTex2       = rangeTex1
  | tex == mageTex1        = mageTex2
  | tex == mageTex2        = mageTex1
  | tex == demonTex1       = demonTex2
  | tex == demonTex2       = demonTex3
  | tex == demonTex3       = demonTex4
  | tex == demonTex4       = demonTex1
  | tex == meleeTexDeath1  = meleeTexDeath2
  | tex == meleeTexDeath2  = meleeTexDeath3
  | tex == meleeTexDeath3  = meleeTexDeath4
  | tex == meleeTexDeath4  = meleeTexDeath4
  | tex == rangeTexDeath1  = rangeTexDeath2
  | tex == rangeTexDeath2  = rangeTexDeath3
  | tex == rangeTexDeath3  = rangeTexDeath3
  | tex == mageTexDeath1   = mageTexDeath2
  | tex == mageTexDeath2   = mageTexDeath3
  | tex == mageTexDeath3   = mageTexDeath4
  | tex == mageTexDeath4   = mageTexDeath5
  | tex == mageTexDeath5   = mageTexDeath6
  | tex == mageTexDeath6   = mageTexDeath7
  | tex == mageTexDeath7   = mageTexDeath7
  | tex == demonTexDeath1  = demonTexDeath2
  | tex == demonTexDeath2  = demonTexDeath3
  | tex == demonTexDeath3  = demonTexDeath4
  | tex == demonTexDeath4  = demonTexDeath4
  | otherwise              = meleeTex1

--if tex is attackTex
checkAttackTex :: ObjectTexture -> Bool
checkAttackTex tex
  | tex == meleeTexAttack2 = True
  | tex == meleeTexAttack1 = True
  | tex == rangeTexAttack  = True
  | tex == mageTexAttack   = True
  | tex == demonTexAttack1 = True
  | tex == demonTexAttack2 = True
  | tex == demonTexAttack3 = True
  | otherwise              = False

--if tex is deathTex
checkDeathTex :: ObjectTexture -> Bool
checkDeathTex tex
  | tex == meleeTexDeath1 = True
  | tex == meleeTexDeath2 = True
  | tex == meleeTexDeath3 = True
  | tex == meleeTexDeath4 = True
  | tex == rangeTexDeath1 = True
  | tex == rangeTexDeath2 = True
  | tex == rangeTexDeath3 = True
  | tex == mageTexDeath1  = True
  | tex == mageTexDeath2  = True
  | tex == mageTexDeath3  = True
  | tex == mageTexDeath4  = True
  | tex == mageTexDeath5  = True
  | tex == mageTexDeath6  = True
  | tex == mageTexDeath7  = True
  | tex == demonTexDeath1 = True
  | tex == demonTexDeath2 = True
  | tex == demonTexDeath3 = True
  | tex == demonTexDeath4 = True
  | otherwise             = False
