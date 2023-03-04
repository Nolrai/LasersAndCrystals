{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE ImportQualifiedPost #-}
{-# LANGUAGE InstanceSigs #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE NoImplicitPrelude #-}

module GameObject (GameObject (..), ObjectType (..), position, orientation, shape, GameWorld, drawGameWorld, testWorld, makeShine, drawGameShine) where

import Control.Lens
import Data.Complex qualified as Complex (cis, phase)
import Data.Complex.Lens (_imagPart, _realPart)
import Data.HashSet (HashSet, fromList, singleton)
import Data.Monoid (mempty)
import Data.Semigroup ((<>))
import GHC.Enum (Enum)
import GHC.Generics (Generic)
import Graphics.Gloss
import Graphics.Gloss.Data.Point.Arithmetic qualified as G
import Orientation (Orientation (..), asDegrees, asRadians, asSquarePoint)
import Relude (Eq, Float, Floating (..), Fractional (..), Generic, Hashable (..), Integer, Num (..), Show, error, foldMap, realToFrac, ($), (.))
import Relude.Enum (Bounded)

data ObjectType
  = Source
  | Mirror
  deriving stock (Eq, Show, Enum, Bounded, Generic)
  deriving anyclass (Hashable)

-- | A game object is a position, velocity, and what type of object it is.
data GameObject = GameObject
  { _position :: !Point,
    _orientation :: !Orientation,
    _shape :: !ObjectType
  }
  deriving stock (Eq, Show, Generic)
  deriving anyclass (Hashable)

makeLenses ''GameObject

x' :: Lens' GameObject Float
x' = position . _1

y' :: Lens' GameObject Float
y' = position . _2

drawGameObject :: GameObject -> Picture
drawGameObject gameObject =
  translate (gameObject ^. x') (gameObject ^. y')
    . rotate (gameObject ^. orientation . re asDegrees)
    $ drawRawGameObject (gameObject ^. shape)

drawRawGameObject :: ObjectType -> Picture
drawRawGameObject Source =
  color red $
    arcSolid 15 (-15) 10
drawRawGameObject Mirror =
  color white $
    rotate 180 $
      rectangleUpperSolid 10 02

type GameWorld = HashSet GameObject

data GameShine = GameShine
  { _gameWorld :: !GameWorld,
    _shine :: !(HashSet Path)
  }
  deriving stock (Eq, Show, Generic)
  deriving anyclass (Hashable)

makeLenses ''GameShine

testWorld :: GameWorld
testWorld = fromList [GameObject (0, 0) East Source, GameObject (100, 0) NorthEast Mirror]

drawGameWorld :: GameWorld -> Picture
drawGameWorld = foldMap drawGameObject

makeShine :: GameWorld -> GameShine
makeShine gw = GameShine gw shine
  where
    shine = foldMap (makeShine' gw) gw

makeShine' :: GameWorld -> GameObject -> HashSet Path
makeShine' gw go =
  case go ^. shape of
    Source -> singleton (startRay gw (go ^. position) (go ^. orientation))
    Mirror -> mempty

startRay :: GameWorld -> Point -> Orientation -> Path
startRay gw pos ori = [pos, pos G.+ ray]
  where
    ray = 1000 G.* (asSquarePoint # ori)

drawGameShine :: GameShine -> Picture
drawGameShine gs = drawGameWorld (gs ^. gameWorld) <> drawShine (gs ^. shine)

drawShine :: HashSet Path -> Picture
drawShine = foldMap drawPath

drawPath :: Path -> Picture
drawPath = color yellow . line
