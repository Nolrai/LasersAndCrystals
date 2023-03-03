{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE InstanceSigs #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE ImportQualifiedPost #-}
{-# LANGUAGE OverloadedStrings #-}


module GameObject (GameObject, position, orientation, shape, GameWorld, drawGameWorld, testWorld, makeShine, drawGameShine) where

import Control.Lens (Lens', makeLenses, (^.), Iso', iso, (#))
import Data.HashSet (HashSet, singleton, fromList)
import Data.Hashable (Hashable)
import GHC.Generics ( Generic )
import Graphics.Gloss
import Data.Ratio
import Data.Complex (Complex((:+)))
import Data.Complex qualified as Complex (cis, phase)
import Data.Complex.Lens ( _imagPart, _realPart)
import Relude (Eq, Float, Generic, Show, foldMap, ($), (.), Num (..), Integer, Fractional (..), Floating (..), realToFrac, error)
import Data.Semigroup ((<>))

data ObjectType
  = Source
  | Mirror
  deriving stock (Eq, Show, Generic)
  deriving anyclass (Hashable)

-- | A game object is a position, velocity, and what type of object it is.
data GameObject = GameObject
  { _position :: !(Complex Rational),
    _orientation :: !Orientation,
    _shape :: !ObjectType
  }
  deriving stock (Eq, Show, Generic)
  deriving anyclass (Hashable)

makeLenses ''GameObject

x' :: Lens' GameObject Rational
x' = position . _realPart

y' :: Lens' GameObject Rational
y' = position . _imagPart

drawGameObject :: GameObject -> Picture
drawGameObject gameObject =
  translate (realToFrac (gameObject ^. x')) (realToFrac (gameObject ^. y'))
    . rotate (gameObject ^. orientation . asDegrees)
    $ drawRawGameObject (gameObject ^. shape)

drawRawGameObject :: ObjectType -> Picture
drawRawGameObject Source = color red $
  arcSolid 15 (-15) 10

drawRawGameObject Mirror = color white $
   rotate 180 $ rectangleUpperSolid 10 02

type GameWorld = HashSet GameObject

data GameShine = GameShine
  { _gameWorld :: !GameWorld
  , _shine :: !(HashSet Path)
  }
  deriving stock (Eq, Show, Generic)
  deriving anyclass (Hashable)

makeLenses ''GameShine

testWorld :: GameWorld
testWorld = fromList [ GameObject (0 :+ 0) 0 Source, GameObject (100 :+ 0) northeast Mirror ]

drawGameWorld :: GameWorld -> Picture
drawGameWorld = foldMap drawGameObject

makeShine :: GameWorld -> GameShine
makeShine gw = GameShine gw (singleton [(0, 0), (100, 0)])

drawGameShine :: GameShine -> Picture
drawGameShine gs = drawGameWorld (gs ^. gameWorld) <> drawShine (gs ^. shine)

drawShine :: HashSet Path -> Picture
drawShine = foldMap drawPath

drawPath :: Path -> Picture
drawPath = color yellow . line
