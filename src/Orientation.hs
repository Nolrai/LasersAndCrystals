{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE ImportQualifiedPost #-}
{-# LANGUAGE NoImplicitPrelude #-}
{-# OPTIONS_GHC -fprof-auto-calls #-}

-- An Orientation is point on the compass rose.
module Orientation
  ( Orientation (..),
    toDegrees,
    toRadians,
    toUnitPoint,
    toSquarePoint,
    normalizeRadians,
    inOriOf,
    denormalize,
    rotatePiOver2,
  )
where

import Control.Lens
import GHC.Float (asinDouble, asinFloat, double2Float)
import GHC.Generics (Generic)
import Graphics.Gloss (Point)
import Graphics.Gloss.Data.Point.Arithmetic qualified as G
import Relude.Container (Hashable)
import Relude hiding (toList, filter, fromList, singleton, HashSet)
import Text.Printf (printf)
import Utils

data Orientation
  = East
  | NorthEast
  | North
  | NorthWest
  | West
  | SouthWest
  | South
  | SouthEast
  deriving stock (Eq, Show, Generic, Enum)
  deriving anyclass (Hashable)

toDegrees :: Orientation -> Float
toDegrees East = 0
toDegrees NorthEast = 45
toDegrees North = 90
toDegrees NorthWest = 135
toDegrees West = 180
toDegrees SouthWest = 225
toDegrees South = 270
toDegrees SouthEast = 315

toRadians :: Orientation -> Float
toRadians = (* (pi / 180)) . toDegrees

toSquarePoint :: Orientation -> Point
toSquarePoint East = (1, 0)
toSquarePoint NorthEast = (1, 1)
toSquarePoint North = (0, 1)
toSquarePoint NorthWest = (-1, 1)
toSquarePoint West = (-1, 0)
toSquarePoint SouthWest = (-1, -1)
toSquarePoint South = (0, -1)
toSquarePoint SouthEast = (1, -1)

normalizePoint :: Point -> Point
normalizePoint (x, y) = (x / magnitude, y / magnitude)
  where
    magnitude = sqrt (x * x + y * y)

toUnitPoint :: Orientation -> Point
toUnitPoint = normalizePoint . toSquarePoint

segToAngle :: Point -> Point -> Float
segToAngle (x1, y1) (x2, y2) = result
  where
    result = atan2 (y2 - y1) (x2 - x1)

inOriOf :: Orientation -> Point -> Point -> Ordering
inOriOf ori base canidate = result 
  where
    result 
      | base == canidate = EQ
      | abs angle < pi / 4 = LT
      | abs angle < 3 * pi / 4 = EQ
      | otherwise = GT
    angle = normalizeRadians (segToAngle base canidate  - toRadians ori)

-- Normalize an angle to be between -pi and pi
normalizeRadians :: Float -> Float
normalizeRadians angle
  | angle < - pi = normalizeRadians (angle + 2 * pi)
  | angle > pi = normalizeRadians (angle - 2 * pi)
  | otherwise = angle

denormalize :: Int16 -> Float
denormalize x = fromIntegral x / fromIntegral (maxBound :: Int16) * 1000 -- 2000 is a little bit more then half a screen

rotatePiOver2 :: Point -> Point
rotatePiOver2 (x, y) = (-y, x)
