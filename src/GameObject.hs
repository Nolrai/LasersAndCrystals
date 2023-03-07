{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE ImportQualifiedPost #-}
{-# LANGUAGE InstanceSigs #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE NoImplicitPrelude #-}
{-# OPTIONS_GHC -fprof-auto-calls #-}

module GameObject (GameObject (..), ObjectType (..), position, orientation, shape, drawGameObject) where

import Control.Lens
import Data.HashSet (HashSet, fromList, toList, singleton)
import Data.Monoid (mempty)
import Data.Semigroup ((<>))
import Data.List (filter, minimumBy)
import GHC.Enum (Enum)
import GHC.Generics (Generic)
import Graphics.Gloss
import Orientation (Orientation (..), toDegrees, toRadians, toSquarePoint)
import Relude hiding (toList, filter, fromList, singleton, HashSet)
import Relude.Enum (Bounded)
import Data.Maybe
import Control.Applicative
import Text.Printf

data ObjectType
  = Source
  | Mirror
  deriving stock (Eq, Show, Enum, Bounded, Generic)
  deriving anyclass (Hashable)

-- | A game object is a position, and what type of object it is.
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
    . rotate (toDegrees . (^. orientation) $ gameObject)
    $ drawRawGameObject (gameObject ^. shape)

drawRawGameObject :: ObjectType -> Picture
drawRawGameObject Source =
  color red $
    arcSolid 15 (-15) 10
drawRawGameObject Mirror =
  color white $
    rotate 180 $
      rectangleUpperSolid 10 02
