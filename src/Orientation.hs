{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DerivingStrategies #-}

-- An Orientation is point on the compass rose.
module Orientation
  ( Orientation (..),
    asDegrees,
    asRadians,
    asUnitPoint,
    asSquarePoint,
  )
where

import Control.Lens
import GHC.Float (asinDouble, asinFloat, double2Float)
import GHC.Generics (Generic)
import Graphics.Gloss (Point)
import Relude.Container (Hashable)

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

fromDegrees :: Float -> Maybe Orientation
fromDegrees f = fromDegrees' (round f)

fromDegrees' :: Int -> Maybe Orientation
fromDegrees' 0 = Just East
fromDegrees' 45 = Just NorthEast
fromDegrees' 90 = Just North
fromDegrees' 135 = Just NorthWest
fromDegrees' 180 = Just West
fromDegrees' 225 = Just SouthWest
fromDegrees' 270 = Just South
fromDegrees' 315 = Just SouthEast
fromDegrees' _ = Nothing

asDegrees :: Prism' Float Orientation
asDegrees = prism' toDegrees fromDegrees

toRadians :: Orientation -> Float
toRadians = (* (pi / 180)) . toDegrees

fromRadians :: Float -> Maybe Orientation
fromRadians = fromDegrees . (* (180 / pi))

asRadians :: Prism' Float Orientation
asRadians = prism' toRadians fromRadians

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

fromPoint :: Point -> Maybe Orientation
fromPoint (x, y) =
  if magnitude == 0
    then Nothing
    else fromRadians $ asinFloat (y / magnitude)
  where
    magnitude = sqrt (x * x + y * y)

asUnitPoint :: Prism' Point Orientation
asUnitPoint = prism' toUnitPoint fromPoint

asSquarePoint :: Prism' Point Orientation
asSquarePoint = prism' toSquarePoint fromPoint
