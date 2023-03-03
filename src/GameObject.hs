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
import Data.HashSet (HashSet, singleton)
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
  deriving stock (Eq, Show, Generic)
  deriving anyclass (Hashable)

newtype Orientation = Orientation {_toV2 :: Complex Rational}
  deriving stock (Eq, Show, Generic)
  deriving anyclass (Hashable)

makeLenses ''Orientation

asDegrees :: Iso' Orientation Float
asDegrees = iso to' from'
  where
    to' :: Orientation -> Float
    to' (Orientation v) = phase' v * 180 / pi
    from' :: Float -> Orientation
    from' a = Orientation $ cis' (a * pi / 180)

phase' :: Complex Rational -> Float
phase' (a :+ b) = Complex.phase (realToFrac a :+ realToFrac b)

cis' :: Float -> Complex Rational
cis' theta = realToFrac (cos theta) :+ realToFrac (sin theta)

instance Num Orientation where
  (+) :: Orientation -> Orientation -> Orientation
  (+) (Orientation (a :+ b)) (Orientation (c :+ d)) = Orientation
    ((a * c - b * d) :+ (a * d + b * c))

  (-) :: Orientation -> Orientation -> Orientation
  (-) (Orientation (a :+ b)) (Orientation (c :+ d)) = Orientation
    ((a * c + b * d) / (c * c + d * d) :+ (b * c - a * d) / (c * c + d * d))

  (*) :: Orientation -> Orientation -> Orientation
  (*) (Orientation a) (Orientation b) =
      error "Orientation multiplication not defined."
      -- Orientation (a ** b) ???

  negate :: Orientation -> Orientation
  negate (Orientation (a :+ b)) = Orientation (a :+ negate b)

  abs :: Orientation -> Orientation
  abs (Orientation (a :+ b)) = Orientation (abs a :+ abs b)

  signum :: Orientation -> Orientation
  signum (Orientation (a :+ b)) = Orientation (signum a :+ signum b)

  fromInteger :: Integer -> Orientation
  fromInteger a = asDegrees # fromInteger a

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

type GameWorld = HashSet GameObject

data GameShine = GameShine
  { _gameWorld :: !GameWorld
  , _shine :: !(HashSet Path)
  }
  deriving stock (Eq, Show, Generic)
  deriving anyclass (Hashable)

makeLenses ''GameShine

testWorld :: GameWorld
testWorld = singleton (GameObject (0 :+ 0) 0 Source)

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
