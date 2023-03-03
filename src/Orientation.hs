{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE TemplateHaskell #-}

-- An Orientation stores an angle as a complex number not equal to 0.

module Orientation
  ( Orientation,
    asDegrees,
    east,
    west,
    north,
    south,
    northeast,
  )
where

newtype Orientation = Orientation {_toV2 :: Complex Rational}
  deriving stock (Show, Generic)
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

east :: Orientation
east = Orientation (1 :+ 0)

west :: Orientation
west = Orientation ((-1) :+ 0)

north :: Orientation
north = Orientation (0 :+ 1)

south :: Orientation
south = Orientation (0 :+ (-1))

northeast :: Orientation
northeast = Orientation (1 :+ 1)
